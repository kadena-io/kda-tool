{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kadena.SigningTypes where

import Control.Lens hiding ((.=))
import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.Char as Char
import qualified Data.HashMap.Strict as HM
import qualified Data.List.Split as L
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GHC.Generics

import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Parse
-- TODO: Rip out sig data dependency
import Pact.Types.SigData (PublicKeyHex(..))

-- The spec calls this `Signer` but it clashes too much with Pact`s `Signer` type
data CSDSigner = CSDSigner
  { _s_pubKey :: PublicKeyHex
  , _s_userSig :: Maybe UserSig
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON CSDSigner where
  toJSON (CSDSigner (PublicKeyHex pkh) mSig) = object $
    [ "pubKey" .= pkh
    , "sig" .= (_usSig <$> mSig)
    ]

instance FromJSON CSDSigner where
  parseJSON = withObject "Signer" $ \o -> do
    pk <- o .: "pubKey"
    mSig ::(Maybe Text) <- o.:? "sig"
    pure $ CSDSigner pk $ UserSig <$> mSig

--------------------------------------------------------------------------------
newtype SignatureList =
  SignatureList { unSignatureList :: [CSDSigner] }
  deriving (Eq, Ord, Show, Semigroup, Monoid, Generic)

instance ToJSON SignatureList where
  toJSON = toJSONList . unSignatureList

instance FromJSON SignatureList where
  parseJSON = fmap SignatureList . parseJSON

--------------------------------------------------------------------------------
data CommandSigData = CommandSigData
  { _csd_sigs :: SignatureList
  , _csd_cmd :: Text
  } deriving (Eq,Ord,Show,Generic)

instance ToJSON CommandSigData where
  toJSON (CommandSigData s c) = object $
    [ "sigs" .= s
    , "cmd" .= c
    ]

instance FromJSON CommandSigData where
  parseJSON = withObject "CommandSigData" $ \o -> do
    s <- o .: "sigs"
    -- TODO should we validate that this is actually a stringified payload here?
    c <- o .: "cmd"
    pure $ CommandSigData s c

--------------------------------------------------------------------------------
data SigningOutcome =
    SO_Success PactHash
  | SO_Failure Text
  | SO_NoSig
  deriving (Eq,Ord,Show,Generic)

instance ToJSON SigningOutcome where
  toJSON a = case a of
    SO_Success h -> object ["result" .= ("success" :: Text), "hash" .= h ]
    SO_Failure msg -> object ["result" .= ("failure" :: Text), "msg" .= msg ]
    SO_NoSig -> object ["result" .= ("noSig" :: Text)]

instance FromJSON SigningOutcome where
  parseJSON = withObject "SigningOutcome" $ \o -> do
    r <- o .: "result"
    case r::Text of
      "success" -> SO_Success <$> o .: "hash"
      "failure" -> SO_Failure <$> o .: "msg"
      "noSig" -> pure SO_NoSig
      otherwise -> fail "ill-formed SigningOutcome"

data CSDResponse = CSDResponse
  { _csdr_csd :: CommandSigData
  , _csdr_outcome :: SigningOutcome
  } deriving (Eq,Ord,Show, Generic)

instance ToJSON CSDResponse where
  toJSON (CSDResponse csd o) = object $
    [ "commandSigData" .= csd
    , "outcome" .= o
    ]

instance FromJSON CSDResponse where
  parseJSON = withObject "CSDResponse" $ \o -> do
    CSDResponse
      <$> o .: "commandSigData"
      <*> o .: "outcome"

data QuicksignError =
    QuicksignError_Reject
  | QuicksignError_EmptyList
  | QuicksignError_Other Text
  deriving (Eq,Ord,Show,Generic)

instance ToJSON QuicksignError where
  toJSON a = case a of
    QuicksignError_Reject -> object ["type" .= ("reject" :: Text)]
    QuicksignError_EmptyList -> object ["type" .= ("emptyList" :: Text)]
    QuicksignError_Other msg -> object ["type" .= ("other" :: Text)
                            , "msg" .= msg
                            ]

instance FromJSON QuicksignError where
  parseJSON = withObject "QuicksignError" $ \o -> do
    t <- o .: "type"
    case t::Text of
      "reject" -> pure QuicksignError_Reject
      "emptyList" -> pure QuicksignError_EmptyList
      "other" -> fmap QuicksignError_Other $ o .: "msg"
      otherwise -> fail "ill-formed QuicksignError"

--------------------------------------------------------------------------------
commandSigDataToCommand :: CommandSigData -> Either String (Command Text)
commandSigDataToCommand = fmap fst . commandSigDataToParsedCommand

commandSigDataToParsedCommand :: CommandSigData -> Either String (Command Text, Payload PublicMeta ParsedCode)
commandSigDataToParsedCommand (CommandSigData (SignatureList sigList) c) = do
  payload :: Payload PublicMeta ParsedCode <- traverse parsePact =<< A.eitherDecodeStrict' (T.encodeUtf8 c)
  let sigMap = M.fromList $ (\(CSDSigner k v) -> (k, v)) <$> sigList
  -- It is ok to use a map here because we're iterating over the signers list and only using the map for lookup.
      sigs = catMaybes $ map (\signer -> join $ M.lookup (PublicKeyHex $ _siPubKey signer) sigMap) $ _pSigners payload
      h = hash (T.encodeUtf8 c)
  pure (Command c sigs h, payload)

--------------------------------------------------------------------------------
newtype AccountName = AccountName
  { unAccountName :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Smart constructor for account names. The only restriction in the coin
-- contract (as it stands) appears to be that accounts can't be an empty string
mkAccountName :: Text -> Either Text AccountName
mkAccountName n =
  if not (isValidCharset n) then Left "Invalid Character detected. Must be Latin1, no spaces, control characters or '|'"
  else if not (isCorrectSize n) then Left "Incorrect length. Must be between 3 and 256 characters in length."
  else Right $ AccountName n

isCorrectSize :: Text -> Bool
isCorrectSize n = let l = T.length n in l >= 3 && l <= 256

isValidCharset :: Text -> Bool
isValidCharset = T.all isValidAccountNameCharacter

isValidAccountNameCharacter :: Char -> Bool
isValidAccountNameCharacter char = Char.isLatin1 char
  && not ( Char.isControl char ||
           char == '\NUL'
         )

-- | Aeson encoding options for compact encoding.
--
--   We pass on the most compact sumEncoding as it could be unsound for certain types.
--
--   But we assume the following naming of constructor names (sum typs) and
--   field names (records): _TypeName_Blah and _typename_blah.
--
--   In particular we assume that only the string after the last underscore is
--   significant for distinguishing field names/constructor names. If this
--   assumption is not met this encoding might not result in the same decoding.
compactEncoding :: Options
compactEncoding = defaultOptions
    { A.fieldLabelModifier = shortener
    , A.allNullaryToStringTag = True
    , A.constructorTagModifier = shortener
    , A.omitNothingFields = True
    , A.sumEncoding = ObjectWithSingleField
    , A.unwrapUnaryRecords = True
    , A.tagSingleConstructors = False
    }
  where
    -- As long as names are not empty or just underscores this head should be fine:
    shortener = head . reverse . filter (/= "") . L.splitOn "_"
