{-# LANGUAGE DeriveGeneric #-}

module Commands.Cut
  ( cutCommand
  ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO as T
import           Data.Text.Encoding
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           System.Exit
------------------------------------------------------------------------------
import           Types.Env
import           Types.HostPort
import           Types.Node
------------------------------------------------------------------------------

cutCommand :: Env -> HostPort -> IO ()
cutCommand e hp = do
  let le = _env_logEnv e
  en <- getNode le hp
  case en of
    Left er -> putStrLn er >> exitFailure
    Right n -> do
      resp <- nodeGetCut n
      case statusCode $ responseStatus resp of
        200 -> T.putStrLn $ decodeUtf8 $ LB.toStrict $ responseBody resp
        _ -> do
          putStrLn "Got unexpected response from node"
          print resp
          exitFailure

