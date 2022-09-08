{-# LANGUAGE DeriveGeneric #-}

module Commands.Poll
  ( pollCommand
  ) where

------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainwebMeta
import           Chainweb.Api.PactCommand
import           Chainweb.Api.Transaction
import           Control.Error
import           Control.Monad.Trans
import           Data.Aeson
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
import           Types.Node
import           Utils
------------------------------------------------------------------------------

pollCommand :: Env -> NodeCmdArgs -> IO ()
pollCommand e args = do
  logEnv e DebugS $ logStr $ "Parsing transactions from the following files:" <> (show $ _nodeCmdArgs_files args)
  bss <- mapM LB.readFile $ _nodeCmdArgs_files args
  res <- runExceptT $ do
    txs :: [Transaction] <- hoistEither $ first unlines $ parseAsJsonOrYaml bss
    n <- ExceptT $ getNode (_nodeCmdArgs_node args)
    let groups = NE.groupBy ((==) `on` txChain) $ sortBy (comparing txChain) txs
    logEnv e DebugS $ fromStr $ printf "Polling %d commands to %d chains\n" (length txs) (length groups)
    responses <- lift $ mapM (\ts -> pollNode n (txChain $ NE.head ts) (_transaction_hash <$> ts)) groups
    lift $ T.putStrLn $ toS $ encode $ map responseToValue responses
  case res of
    Left er -> putStrLn er >> exitFailure
    Right () -> pure ()

txChain :: Transaction -> ChainId
txChain = _chainwebMeta_chainId . _pactCommand_meta . _transaction_cmd

