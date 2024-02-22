{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
import           Katip
import           Network.Connection
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Text.Printf
------------------------------------------------------------------------------
import           Types.Env
import           Types.HostPort
------------------------------------------------------------------------------

data ServerType = PactServer | ChainwebServer
  deriving (Eq,Ord,Show)

data Node = Node
  { _node_scheme :: Scheme
  , _node_server :: HostPort
  , _node_httpManager :: Manager
  , _node_serverType :: ServerType
  , _node_nodeInfo :: Maybe NodeInfo
  -- ^ mainnet01, testnet04, etc or Nothing if it's a pact -s server
  }

getNodeServiceApi :: LogEnv -> SchemeHostPort -> IO (Either String Node)
getNodeServiceApi le shp = do
    -- TODO Add exception handling from below comment & delete comment

    let hp = _shp_hostPort shp
    case _shp_scheme shp of
      Nothing -> runExceptT $
        tryChainwebInfo le Https hp <|>
        tryChainwebInfo le Http hp <|>
        tryPact le Http hp
        -- Only try http for pact -s servers because that's the overwhelming
        -- default. If a user wants to use https with a pact server, they can
        -- do so by specifying the scheme manually.
      Just s -> runExceptT $
        tryChainwebInfo le s hp <|>
        tryPact le s hp

getNodeP2PApi :: LogEnv -> SchemeHostPort -> IO (Either String Node)
getNodeP2PApi le shp = do
    let hp = _shp_hostPort shp
    case _shp_scheme shp of
      Nothing -> runExceptT $
        tryChainwebInfo le Https hp <|>
        tryChainwebInfo le Http hp
        -- The pact -s server doesn't have a P2P API, so no sense in trying.
      Just s -> runExceptT $ tryChainwebInfo le s hp

tryChainwebInfo
  :: LogEnv
  -> Scheme
  -> HostPort
  -> ExceptT String IO Node
tryChainwebInfo le s hp = do
  logLE le DebugS $ fromStr $ printf "tryChainwebInfo %s %s" (show s) (hostPortToText hp)
  (mgr,ni) <- queryEndpoint le s hp "/info"
  pure (Node s hp mgr ChainwebServer ni)

tryPact
  :: LogEnv
  -> Scheme
  -> HostPort
  -> ExceptT String IO Node
tryPact le s hp = do
  logLE le DebugS $ fromStr $ printf "tryChainwebInfo %s %s" (show s) (hostPortToText hp)
  (mgr,_ :: Value) <- queryEndpoint le s hp "/version"
  pure (Node s hp mgr PactServer Nothing)

queryEndpoint :: FromJSON a => LogEnv -> Scheme -> HostPort -> Text -> ExceptT String IO (Manager, a)
queryEndpoint le s hp urlPath = do
    (mgr, respBody) <- queryHostPort le s hp urlPath
    case eitherDecode respBody of
      Left e -> throwE $ unlines
        [ "Error decoding NodeInfo: " <> e
        , toS respBody
        ]
      Right a -> pure (mgr, a)

queryHostPort :: LogEnv -> Scheme -> HostPort -> Text -> ExceptT String IO (Manager, LB.ByteString)
queryHostPort le s hp urlPath = ExceptT $ do
    mgr <- case s of
      Http -> newManager defaultManagerSettings
      Https -> newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
    let shpText = schemeHostPortToText $ SchemeHostPort (Just s) hp
        url = T.unpack $ shpText <> urlPath
    logLE le DebugS $ logStr $ "Parsing url " <> url
    req <- parseRequest url
    logFLE le DebugS req "queryHostPort"
    resp <- httpLbs req mgr
    let status = responseStatus resp
    return $ if statusIsSuccessful status
      then Right $ (mgr, responseBody resp)
      else Left $ printf "Got HTTP status %d from %s" (statusCode status) shpText

chainwebApiRoot :: Scheme -> HostPort -> Text -> Text -> Text
chainwebApiRoot s hp apiVer networkId =
    prefix <>
    "chainweb/" <>
    apiVer <> "/" <>
    networkId
  where
    hpText = schemeHostPortToText $ SchemeHostPort (Just s) hp
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
    hpText = schemeHostPortToText $ SchemeHostPort (Just $ _node_scheme n) (_node_server n)
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

nodeGetCut :: LogEnv -> SchemeHostPort -> Text -> Text -> IO (Response LB.ByteString)
nodeGetCut le shp apiVer networkId = do
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
    logFLE le DebugS req "nodeGetCut"
    httpLbs req mgr
  where
    s = fromMaybe Https $ _shp_scheme shp
    url = T.unpack root <> "/cut"
    root = chainwebApiRoot s (_shp_hostPort shp) apiVer networkId

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

pollNode :: LogEnv -> Node -> Text -> NE.NonEmpty Hash -> IO (Response LB.ByteString)
pollNode le n cid rks = do
    req0 <- parseRequest url
    let bs = encode $ object [ "requestKeys" .= rks ]
    let req = req0
          { method = "POST"
          , requestBody = RequestBodyLBS bs
          , requestHeaders = [(hContentType, "application/json")]
          }
    logFLE le DebugS req "sending poll"
    httpLbs req (_node_httpManager n)
  where
    url = T.unpack root <> "/poll"
    root = nodePactRoot n cid

sendToNode :: LogEnv -> Node -> NE.NonEmpty Transaction -> IO (Response LB.ByteString)
sendToNode le n ts@(t NE.:| _) = do
    req0 <- parseRequest url
    let bs = encode $ object [ "cmds" .= ts ]
    let req = req0
          { method = "POST"
          , requestBody = RequestBodyLBS bs
          , requestHeaders = [(hContentType, "application/json")]
          }
    logFLE le DebugS req "sending send"
    httpLbs req (_node_httpManager n)
  where
    url = T.unpack root <> "/send"
    root = nodePactRoot n $ _chainwebMeta_chainId $ _pactCommand_meta $ _transaction_cmd t

localNodeQuery :: LogEnv -> Bool -> Node -> Transaction -> IO (Response LB.ByteString)
localNodeQuery le sigVerification n t = do
    req0 <- parseRequest url
    let req = req0
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode t
          , requestHeaders = [(hContentType, "application/json")]
          }
    logFLE le DebugS req "sending local"
    httpLbs req (_node_httpManager n)
  where
    url = T.unpack root <> addQS "/local"
    addQS p = if sigVerification then p else p <> "?signatureVerification=false&preflight=true"
    root = nodePactRoot n $ _chainwebMeta_chainId $ _pactCommand_meta $ _transaction_cmd t

responseToValue :: Response LB.ByteString -> Value
responseToValue r = do
    case eitherDecode $ responseBody r of
      Left e -> object
        [ "error" .= e
        , "nodeResponse" .= decodeUtf8 (LB.toStrict (responseBody r))
        ]
      Right (v :: Value) -> object
        [ "statusCode" .= statusCode s
        , "statusMsg" .= decodeUtf8 (statusMessage s)
        , "body" .= v
        ]
  where
    s = responseStatus r
