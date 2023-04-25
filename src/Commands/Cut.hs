{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Cut
  ( cutCommand
  ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Text (Text)
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

cutCommand :: Env -> SchemeHostPort -> Maybe Text -> Maybe Text -> IO ()
cutCommand e shp mApiVer mNetwork = do
  let le = _env_logEnv e
  resp <- nodeGetCut le shp (fromMaybe "0.0" mApiVer) (fromMaybe "mainnet01" mNetwork)
  case statusCode $ responseStatus resp of
    200 -> T.putStrLn $ decodeUtf8 $ LB.toStrict $ responseBody resp
    _ -> do
      putStrLn "Got unexpected response from node"
      print resp
      exitFailure
