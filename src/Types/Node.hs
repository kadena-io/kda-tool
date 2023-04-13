{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Types.Node where

------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainwebMeta
import           Chainweb.Api.Hash
import           Chainweb.Api.NodeInfo
import           Chainweb.Api.PactCommand
import           Chainweb.Api.Transaction
import           Control.Applicative
import           Control.Error
import           Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.List.NonEmpty as NE
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Network.Connection
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Text.Printf
------------------------------------------------------------------------------
import           Types.HostPort
------------------------------------------------------------------------------

data ServerType = PactServer | ChainwebServer
  deriving (Eq,Ord,Show)

data Node = Node
  { _node_server :: HostPort
  , _node_httpManager :: Manager
  , _node_serverType :: ServerType
  , _node_nodeInfo :: Maybe NodeInfo
  -- ^ mainnet01, testnet04, etc or Nothing if it's a pact -s server
  }

getNodeServiceApi :: HostPort -> IO (Either String Node)
getNodeServiceApi hp = do
    epair <- runExceptT $
      tryChainweb Https hp <|>
      tryChainweb Http hp <|>
      tryPact hp
    pure $ case epair of
      Left _ -> Left $ T.unpack $ hostPortToText hp <> " does not expose a service API"
      Right (mgr,st,ni) -> Right $ Node hp mgr st ni

getNodeP2PApi :: HostPort -> IO (Either String Node)
getNodeP2PApi hp = do
    epair <- runExceptT $
      tryChainweb Https hp <|>
      tryChainweb Http hp <|>
      tryPact hp
    pure $ case epair of
      Left _ -> Left $ T.unpack $ hostPortToText hp <> " does not expose a service API"
      Right (mgr,st,ni) -> Right $ Node hp mgr st ni

tryChainweb :: Scheme -> HostPort -> ExceptT String IO (Manager, ServerType, Maybe NodeInfo)
tryChainweb s hp = do
  (mgr,ni) <- queryEndpoint s hp "/info"
  pure (mgr, ChainwebServer, ni)

tryPact :: HostPort -> ExceptT String IO (Manager, ServerType, Maybe NodeInfo)
tryPact hp = do
  (mgr,_ :: Value) <- queryEndpoint Http hp "/version"
  pure (mgr, PactServer, Nothing)

queryEndpoint :: FromJSON a => Scheme -> HostPort -> Text -> ExceptT String IO (Manager, a)
queryEndpoint s hp urlPath = do
    (mgr, respBody) <- queryHostPort hp s urlPath
    case eitherDecode respBody of
      Left e -> throwE $ unlines
        [ "Error decoding NodeInfo: " <> e
        , toS respBody
        ]
      Right a -> pure (mgr, a)

queryHostPort :: HostPort -> Scheme -> Text -> ExceptT String IO (Manager, LB.ByteString)
queryHostPort hp s urlPath = ExceptT $ do
    mgr <- case s of
      Http -> newManager defaultManagerSettings
      Https -> newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
    let hpText = hostPortToText hp
        url = T.unpack $ hpText <> urlPath
    req <- parseRequest url
    resp <- httpLbs req mgr
    let status = responseStatus resp
    return $ if statusIsSuccessful status
      then Right $ (mgr, responseBody resp)
      else Left $ printf "Got HTTP status %d from %s" (statusCode status) hpText

chainwebApiRoot :: HostPort -> Text -> Text -> Text
chainwebApiRoot hp apiVer networkId =
    prefix <>
    "chainweb/" <>
    apiVer <> "/" <>
    networkId
  where
    hpText = hostPortToText hp
    prefix = hpText <> "/"

nodeApiRoot :: Node -> Text
nodeApiRoot n =
    case (_node_serverType n, _node_nodeInfo n) of
      (PactServer, _) -> prefix
      (ChainwebServer, Just ni) ->
        prefix <>
        "chainweb/" <>
        _nodeInfo_apiVer ni <> "/" <>
        _nodeInfo_chainwebVer ni
      _ -> error $ "Couldn't get node " <> T.unpack hpText <> " info"
  where
    hpText = hostPortToText (_node_server n)
    prefix = hpText <> "/"

nodeChainRoot :: Node -> Text -> Text
nodeChainRoot n c =
    case (_node_serverType n) of
      PactServer -> nodeApiRoot n
      ChainwebServer ->
        nodeApiRoot n <>
        "/chain/" <> c

nodePactRoot :: Node -> Text -> Text
nodePactRoot n c =
    case (_node_serverType n) of
      PactServer -> nodeApiRoot n
      ChainwebServer ->
        nodeChainRoot n c <>
        "/pact/api/v1"

nodeGetCut :: SchemeHostPort -> Text -> Text -> IO (Response LB.ByteString)
nodeGetCut shp apiVer networkId = do
    mgr <- case _shp_scheme shp of
      Just Http -> newManager defaultManagerSettings
      -- Default to https for cuts if there was no explicit endpoint
      _ -> newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
    req0 <- parseRequest url
    let req = req0
          { method = "GET"
--          , requestBody = RequestBodyLBS bs
--          , requestHeaders = [(hContentType, "application/json")]
          }
    httpLbs req mgr
  where
    url = T.unpack root <> "/cut"
    root = chainwebApiRoot (_shp_hostPort shp) apiVer networkId

-- | This has to take a HostPort instead of a Node because you might want to
-- query the mempool on nodes that don't expose the service API which has the
-- /info endpoint that is needed to construct a 'Node'.
mempoolPending :: SchemeHostPort -> Text -> Text -> ChainId -> IO (Response LB.ByteString)
mempoolPending shp apiVer network c = do
    req0 <- parseRequest url
    let req = req0
          { method = "POST"
          , requestHeaders = [(hContentType, "application/json")]
          }
    mgr <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
    httpLbs req mgr
  where
    url = printf "%s/chainweb/%s/%s/chain/%d/mempool/getPending" (schemeHostPortToText shp) apiVer network (unChainId c)

pollNode :: Node -> Text -> NE.NonEmpty Hash -> IO (Response LB.ByteString)
pollNode n cid rks = do
    req0 <- parseRequest url
    let bs = encode $ object [ "requestKeys" .= rks ]
    let req = req0
          { method = "POST"
          , requestBody = RequestBodyLBS bs
          , requestHeaders = [(hContentType, "application/json")]
          }
    httpLbs req (_node_httpManager n)
  where
    url = T.unpack root <> "/poll"
    root = nodePactRoot n cid

sendToNode :: Node -> NE.NonEmpty Transaction -> IO (Response LB.ByteString)
sendToNode n ts@(t NE.:| _) = do
    req0 <- parseRequest url
    let bs = encode $ object [ "cmds" .= ts ]
    let req = req0
          { method = "POST"
          , requestBody = RequestBodyLBS bs
          , requestHeaders = [(hContentType, "application/json")]
          }
    httpLbs req (_node_httpManager n)
  where
    url = T.unpack root <> "/send"
    root = nodePactRoot n $ _chainwebMeta_chainId $ _pactCommand_meta $ _transaction_cmd t

localNodeQuery :: Node -> Transaction -> IO (Response LB.ByteString)
localNodeQuery n t = do
    req0 <- parseRequest url
    let req = req0
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode t
          , requestHeaders = [(hContentType, "application/json")]
          }
    httpLbs req (_node_httpManager n)
  where
    url = T.unpack root <> "/local"
    root = nodePactRoot n $ _chainwebMeta_chainId $ _pactCommand_meta $ _transaction_cmd t

responseToValue :: Response LB.ByteString -> Value
responseToValue r = do
    case eitherDecode $ responseBody r of
      Left e -> object [ "error" .= e ]
      Right (v :: Value) -> object
        [ "statusCode" .= statusCode s
        , "statusMsg" .= decodeUtf8 (statusMessage s)
        , "body" .= v
        ]
  where
    s = responseStatus r
