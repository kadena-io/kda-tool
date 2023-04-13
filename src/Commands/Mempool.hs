{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Mempool
  ( mempoolCommand
  ) where

------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Data.Text.Encoding
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           System.Exit
------------------------------------------------------------------------------
import           Types.HostPort
import           Types.Node
------------------------------------------------------------------------------

mempoolCommand :: SchemeHostPort -> Maybe Text -> Maybe Text -> ChainId -> IO ()
mempoolCommand shp mApiVer mNetwork c = do
  let shp2 = case shp of
        SchemeHostPort Nothing hp -> SchemeHostPort (Just Https) hp
        _ -> shp
  resp <- mempoolPending shp2 (fromMaybe "0.0" mApiVer) (fromMaybe "mainnet01" mNetwork) c
  case statusCode $ responseStatus resp of
    200 -> T.putStrLn $ decodeUtf8 $ LB.toStrict $ responseBody resp
    _ -> do
      putStrLn "Got unexpected response from node"
      print resp
      exitFailure

