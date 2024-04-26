{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Kadena.SigningApi where

import Control.Lens hiding ((.=))
import Control.Applicative((<|>))
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Pact.Server.API
import Pact.Types.Capability (SigCapability(..))
import Pact.Types.ChainMeta (TTLSeconds(..))
import Pact.Types.Runtime (GasLimit(..), ChainId, PublicKeyText)
import Pact.Types.Command (Command)
import Servant.API

import Kadena.SigningTypes

-- | Values of this type are supplied by the dapp author to the wallet so the
-- wallet knows what capabilities need to be granted for the transaction.
data DappCap = DappCap
  { _dappCap_role :: Text
  -- ^ Short name for this capability that is meaningful to the user
  , _dappCap_description :: Text
  -- ^ More detailed information that the user might need to know
  , _dappCap_cap :: SigCapability
  -- ^ The actual capability
  } deriving (Eq,Ord,Show,Generic)

instance ToJSON DappCap where
  toJSON = genericToJSON compactEncoding
  toEncoding = genericToEncoding compactEncoding

instance FromJSON DappCap where
  parseJSON = genericParseJSON compactEncoding

data SigningRequest = SigningRequest
  { _signingRequest_code :: Text
  , _signingRequest_data :: Maybe Object
  , _signingRequest_caps :: [DappCap]
  , _signingRequest_nonce :: Maybe Text
  , _signingRequest_chainId :: Maybe ChainId
  , _signingRequest_gasLimit :: Maybe GasLimit
  , _signingRequest_ttl :: Maybe TTLSeconds
  , _signingRequest_sender :: Maybe AccountName
  , _signingRequest_extraSigners :: Maybe [PublicKeyText]
  } deriving (Show, Generic)

instance ToJSON SigningRequest where
  toJSON = genericToJSON compactEncoding
  toEncoding = genericToEncoding compactEncoding

instance FromJSON SigningRequest where
  parseJSON = genericParseJSON compactEncoding

data SigningResponse = SigningResponse
  { _signingResponse_body :: Command Text
  , _signingResponse_chainId :: ChainId
  } deriving (Eq, Show, Generic)

instance ToJSON SigningResponse where
  toJSON = genericToJSON compactEncoding
  toEncoding = genericToEncoding compactEncoding

instance FromJSON SigningResponse where
  parseJSON = genericParseJSON compactEncoding

--------------------------------------------------------------------------------
newtype QuickSignRequest = QuickSignRequest
  { _quickSignRequest_csds :: [CommandSigData]
  } deriving (Show, Eq, Generic)

instance ToJSON QuickSignRequest where
  toJSON a = object ["cmdSigDatas" .= _quickSignRequest_csds a]

instance FromJSON QuickSignRequest where
  parseJSON = withObject "QuickSignRequest" $ \o -> do
    cmd <- o .: "cmdSigDatas"
    pure $ QuickSignRequest cmd

data QuickSignResponse =
    QSR_Response [CSDResponse]
  | QSR_Error QuicksignError
  deriving (Show, Eq, Generic)

instance ToJSON QuickSignResponse where
  toJSON a = case a of
    QSR_Response responses -> object ["responses" .= responses]
    QSR_Error e -> object ["error" .= e]

instance FromJSON QuickSignResponse where
  parseJSON = withObject "QuickSignResponse" $ \o -> do
    (fmap QSR_Response $ o .: "responses")
    <|> (fmap QSR_Error $ o.: "error")
--------------------------------------------------------------------------------

type SigningApi = "v1" :> V1SigningApi
type V1SigningApi = "sign" :> ReqBody '[JSON] SigningRequest :> Post '[JSON] SigningResponse
               :<|> "quicksign" :> ReqBody '[JSON] QuickSignRequest :> Post '[JSON] QuickSignResponse

signingAPI :: Proxy SigningApi
signingAPI = Proxy

