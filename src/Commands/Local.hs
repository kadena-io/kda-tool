{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Local
  ( localCommand
  ) where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Key
import           Data.Aeson.Lens
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import           Data.Function
import           Data.List
import qualified Data.List.NonEmpty as NE
import           Data.Ord
import           Data.String.Conv
import           Katip
import           System.Exit
import           Text.Printf
------------------------------------------------------------------------------
import           Types.Env
import           Types.HostPort
import           Types.Node
import           Utils
------------------------------------------------------------------------------

localCommand :: Env -> LocalCmdArgs -> IO ()
localCommand e (LocalCmdArgs args verifySigs shortOutput) = do
  let le = _env_logEnv e
  case _nodeTxCmdArgs_files args of
    [] -> putStrLn "No tx files specified"
    fs -> do
      logEnv e DebugS $ logStr $ "Parsing transactions from the following files:" <> (show $ _nodeTxCmdArgs_files args)
      bss <- mapM LB.readFile fs
      res <- runExceptT $ do
        allTxs <- hoistEither $ first unlines $ parseAsJsonOrYaml verifySigs bss
        shpPairs <- handleOptionalNode e allTxs $ _nodeTxCmdArgs_node args
        forM shpPairs $ \(shp, txs) -> do
          n <- ExceptT $ getNodeServiceApi le shp
          let groups = NE.groupBy ((==) `on` txChain) $ sortBy (comparing txChain) txs
          logEnv e DebugS $ fromStr $
            printf "%s: testing %d commands on %d chains\n"
              (schemeHostPortToText shp) (length txs) (length groups)
          responses <- lift $ mapM (localNodeQuery le verifySigs n) txs
          pure $ fromText (schemeHostPortToText shp) .= map responseToValue responses
      case res of
        Left er -> putStrLn er >> exitFailure
        Right results -> do
          let out = Object $ mconcat results
          let status = out ^.. _Object . traverse . _Array . traverse . key "body" . key "result" . key "status" . _String
          if shortOutput
            then putStrLn $ toS $ encode status
            else putStrLn $ toS $ encode out
          when (any (/="success") status) $
            exitWith (ExitFailure 2)
