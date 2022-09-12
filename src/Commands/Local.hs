{-# LANGUAGE DeriveGeneric #-}

module Commands.Local
  ( localCommand
  ) where

------------------------------------------------------------------------------
import           Chainweb.Api.Transaction
import           Control.Error
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
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

localCommand :: Env -> NodeCmdArgs -> IO ()
localCommand e args = do
  case _nodeCmdArgs_files args of
    [] -> putStrLn "No tx files specified"
    fs -> do
      logEnv e DebugS $ logStr $ "Parsing transactions from the following files:" <> (show $ _nodeCmdArgs_files args)
      bss <- mapM LB.readFile fs
      res <- runExceptT $ do
        txs :: [Transaction] <- hoistEither $ first unlines $ parseAsJsonOrYaml bss
        n <- ExceptT $ getNode (_nodeCmdArgs_node args)
        logEnv e DebugS $ fromStr $ printf "Testing %d commands locally\n" (length txs)
        responses <- lift $ mapM (localNodeQuery n) txs
        lift $ T.putStrLn $ toS $ encode $ map responseToValue responses
      case res of
        Left er -> putStrLn er >> exitFailure
        Right () -> pure ()
