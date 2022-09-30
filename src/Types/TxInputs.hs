{-# LANGUAGE OverloadedStrings #-}

module Types.TxInputs where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Data.Aeson as A
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Pact.ApiReq
import           Pact.Types.Lang
import           Pact.Types.RPC
import           Pact.Types.Runtime
------------------------------------------------------------------------------

data PactTxType = PttExec | PttCont
  deriving (Eq,Ord,Show,Read)

ptt2text :: PactTxType -> Text
ptt2text PttExec = "exec"
ptt2text PttCont = "cont"

instance ToJSON PactTxType where
  toJSON = String . ptt2text

instance FromJSON PactTxType where
  parseJSON = withText "PactTxType" $ \t ->
    case t of
      "exec" -> pure PttExec
      "cont" -> pure PttCont
      _ -> fail ("Error: " <> T.unpack t <> " not a valid PactTxType")

data ExecInputs = ExecInputs
  { _execInputs_codeOrFile :: Either Text FilePath
  , _execInputs_dataOrFile :: Either Value FilePath
  } deriving (Eq,Show)

execInputsPairs :: (Monoid a, KeyValue a) => ExecInputs -> a
execInputsPairs ei = mconcat
  [ either ("code" .=) ("codeFile" .=) $ _execInputs_codeOrFile ei
  , either ("data" .=) ("dataFile" .=) $ _execInputs_dataOrFile ei
  ]

-- | This type makes the following (backwards-compatible) changes from ApiReq in
-- Pact:
--
-- The "networkId" field is now mandatory
--
-- The "publicMeta" field is changed to "meta" to match the name that is used in
--   the final commands that are submitted to the blockchain but the FromJSON
--   instance still accepts "publicMeta".
--
-- This same publicMeta/meta field is now mandatory
data TxInputs = TxInputs
  { _txInputs_type :: PactTxType
  , _txInputs_payload :: Either ContMsg ExecInputs
  , _txInputs_signers :: [ApiSigner]
  , _txInputs_nonce :: Maybe Text
  , _txInputs_meta :: ApiPublicMeta
  , _txInputs_networkId :: NetworkId
  } deriving (Eq,Show)

txInputsToApiReq :: TxInputs -> IO ApiReq
txInputsToApiReq txi = do
  let t = ptt2text $ _txInputs_type txi
      n = Just $ _txInputs_networkId txi
  case _txInputs_payload txi of
    Left c -> pure $ ApiReq
      (Just t)
      (let PactId t = _cmPactId c in hush $ fromText' t)
      (Just $ _cmStep c)
      (Just $ _cmRollback c)
      (Just $ _cmData c)
      (_cmProof c)
      Nothing
      Nothing
      Nothing
      Nothing
      (Just $ _txInputs_signers txi)
      (_txInputs_nonce txi)
      (Just $ _txInputs_meta txi)
      (Just $ _txInputs_networkId txi)
    Right ei -> do
      d <- getOrReadFile (eitherDecode . LB.fromStrict . T.encodeUtf8) $ _execInputs_dataOrFile ei
      c <- getOrReadFile Right $ _execInputs_codeOrFile ei
      pure $ ApiReq
        (Just t)
        Nothing
        Nothing
        Nothing
        (hush d)
        Nothing
        Nothing
        (hush c)
        Nothing
        Nothing
        (Just $ _txInputs_signers txi)
        (_txInputs_nonce txi)
        (Just $ _txInputs_meta txi)
        (Just $ _txInputs_networkId txi)

getOrReadFile :: (Text -> Either String a) -> Either a FilePath -> IO (Either String a)
getOrReadFile parser (Left a) = pure $ Right a
getOrReadFile parser (Right fp) = do
  t <- T.readFile fp
  pure $ parser t

instance ToJSON TxInputs where
  toJSON ti = A.Object $ payloadPairs <> mconcat
    [ "type" .= _txInputs_type ti
    , "signers" .= _txInputs_signers ti
    , "nonce" .?= _txInputs_nonce ti

    -- TODO Not sure if this should be "meta" or "publicMeta". I think it should
    -- be "meta" because we want to move people towards the key used in the
    -- actual Pact API and people shouldn't be consuming the output of this
    -- function with a legacy pact command line executable.
    , "meta" .= _txInputs_meta ti

    , "networkId" .= _txInputs_networkId ti
    ]
    where
      payloadPairs = either contMsgJsonPairs execInputsPairs $ _txInputs_payload ti
      k .?= v = case v of
        Nothing -> mempty
        Just v' -> k .= v'


instance FromJSON TxInputs where
  parseJSON v = withObject "TxInputs" func v
    where
      func o = do
        t <- pure . fromMaybe PttExec =<< o .:? "type"
        p <- case t of
          PttCont -> Left <$> parseJSON v
          PttExec -> do
            mc :: Maybe (Either Text FilePath) <- parseMaybePair o "code"
            md :: Maybe (Either Value FilePath) <- parseMaybePair o "data"
            c <- maybe (fail "Must have exec or cont fields") pure mc
            let d = fromMaybe (Left $ object []) md
            pure $ Right $ ExecInputs c d

        -- We want to allow both "meta" and "publicMeta" here to make this tool
        -- usable in as many situations as possible. We can consider removing
        -- the legacy support for "publicMeta" somewhere down the line after
        -- sufficient adoption.
        m <- (o .: "meta") <|> (o .: "publicMeta")

        TxInputs
          <$> pure t
          <*> pure p
          <*> o .: "signers"
          <*> o .:? "nonce"
          <*> pure m
          <*> o .: "networkId"

parseMaybePair
  :: FromJSON a
  => A.Object
  -> Text
  -> Parser (Maybe (Either a FilePath))
parseMaybePair o name = do
  mn <- o .:? name
  let nameFile = name <> "File"
  mf <- o .:? nameFile
  case (mn,mf) of
    (Nothing,Nothing) -> pure Nothing
    (Just n,Nothing) -> pure $ Just $ Left n
    (Nothing,Just f) -> pure $ Just $ Right f
    (Just _,Just _) -> fail $ T.unpack ("Cannot have both " <> name <> " and " <> nameFile)
