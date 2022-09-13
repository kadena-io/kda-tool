{-# LANGUAGE DeriveGeneric #-}

module Commands.Mempool
  ( mempoolCommand
  ) where

------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import qualified Data.ByteString.Lazy as LB
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

mempoolCommand :: HostPort -> Text -> ChainId -> IO ()
mempoolCommand hp network c = do
  resp <- mempoolPending hp network c
  case statusCode $ responseStatus resp of
    200 -> T.putStrLn $ decodeUtf8 $ LB.toStrict $ responseBody resp
    _ -> do
      putStrLn "Got unexpected response from node"
      print resp
      exitFailure

