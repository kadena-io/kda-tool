{-# LANGUAGE DeriveGeneric #-}

module Commands.Send
  ( sendCommand
  ) where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Key
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import           Data.Function
import           Data.List
import qualified Data.List.NonEmpty as NE
import           Data.Ord
import           Data.String.Conv
import qualified Data.Text.IO as T
import           Katip
import           System.Exit
import           Text.Printf
------------------------------------------------------------------------------
import           Types.Env
import           Types.HostPort
import           Types.Node
import           Utils
------------------------------------------------------------------------------

sendCommand :: Env -> NodeTxCmdArgs -> IO ()
sendCommand e args = do
  let le = _env_logEnv e
  case _nodeTxCmdArgs_files args of
    [] -> putStrLn "No tx files specified"
    fs -> do
      logEnv e DebugS $ logStr $ "Parsing transactions from the following files:" <> (show $ _nodeTxCmdArgs_files args)
      bss <- mapM LB.readFile fs
      res <- runExceptT $ do
        allTxs <- hoistEither $ first unlines $ parseAsJsonOrYaml True bss
        shpPairs <- handleOptionalNode e allTxs $ _nodeTxCmdArgs_node args
        let numNets = length shpPairs
        logEnv e InfoS $ fromStr $ printf "Sending %d transactions to %d %s"
          (length allTxs) numNets (if numNets == 1 then "network" else "networks")
        forM shpPairs $ \(shp, txs) -> do
          n <- ExceptT $ getNodeServiceApi le shp
          let groups = NE.groupBy ((==) `on` txChain) $ sortBy (comparing txChain) txs
          logEnv e DebugS $ fromStr $
            printf "%s: sending %d commands to %d chains\n"
              (schemeHostPortToText shp) (length txs) (length groups)
          responses <- lift $ mapM (sendToNode le n) groups
          pure $ fromText (schemeHostPortToText shp) .= map responseToValue responses
      case res of
        Left er -> putStrLn er >> exitFailure
        Right results -> T.putStrLn $ toS $ encode $ Object $ mconcat results
