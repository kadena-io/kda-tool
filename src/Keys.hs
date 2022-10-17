{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Keys where

------------------------------------------------------------------------------
import qualified Cardano.Crypto.Wallet as Crypto
import           Control.Error
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Crypto.Encoding.BIP39 as Crypto
import qualified Crypto.Encoding.BIP39.English as Crypto
import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as ED25519
import qualified Crypto.Random.Entropy
import           Data.Aeson
import           Data.Bifunctor
import           Data.Bits ((.|.))
import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map as Map
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Word (Word32)
import qualified Data.YAML.Aeson as YA
import           GHC.Natural
import           System.IO
import           System.IO.Echo
import           Text.Read (readMaybe)
------------------------------------------------------------------------------
import           Utils
------------------------------------------------------------------------------

mnemonicToRoot :: MnemonicPhrase -> Crypto.XPrv
mnemonicToRoot phrase = seedToRoot (phraseToSeed phrase) "" -- TODO: Empty passowrd

genMnemonic12 :: MonadIO m => m (Either Text (Crypto.MnemonicSentence 12))
genMnemonic12 = liftIO $ bimap tshow Crypto.entropyToWords . Crypto.toEntropy @128
  -- This size must be a 1/8th the size of the 'toEntropy' size: 128 / 8 = 16
  <$> Crypto.Random.Entropy.getEntropy @ByteString 16

generateCryptoPairFromRoot :: Crypto.XPrv -> Text -> KeyIndex -> (EncryptedPrivateKey, PublicKey)
generateCryptoPairFromRoot root pass i =
  let hardenedIdx = 0x80000000 .|. (fromKeyIndex i)
      xprv = Crypto.deriveXPrv scheme (T.encodeUtf8 pass) root hardenedIdx
  in (EncryptedPrivateKey xprv, PublicKey $ Crypto.xpubPublicKey $ Crypto.toXPub xprv)
  where
    scheme = Crypto.DerivationScheme2

mkPhraseMapFromMnemonic
  :: forall mw.
     Crypto.ValidMnemonicSentence mw
  => Crypto.MnemonicSentence mw
  -> Map.Map WordKey Text
mkPhraseMapFromMnemonic = wordsToPhraseMap . T.words . baToText
  . Crypto.mnemonicSentenceToString @mw Crypto.english

newtype MnemonicPhrase = MnemonicPhrase [ Text ]
  deriving (Show, Eq)

-- TODO Allow 24-word phrases
mkMnemonicPhrase :: [Text] -> Maybe MnemonicPhrase
mkMnemonicPhrase lst
  | length lst == 12 = Just $ MnemonicPhrase lst
  | otherwise = Nothing

readPhraseFromFile :: FilePath -> IO (Maybe MnemonicPhrase)
readPhraseFromFile keyfile = mkMnemonicPhrase . T.words . T.strip <$> T.readFile keyfile

readPhraseFromHandle :: Handle -> IO (Maybe MnemonicPhrase)
readPhraseFromHandle h = mkMnemonicPhrase . T.words . T.strip <$> T.hGetContents h

-- TODO: Don't expose constructor; only create with 'mkKeyIndex'
newtype KeyIndex = KeyIndex { unKeyIndex :: Natural }
  deriving (Eq, Ord, Show, Read, Num, Enum)

fromKeyIndex :: KeyIndex -> Word32
fromKeyIndex = fromIntegral . naturalToInt . unKeyIndex

-- genMnemonic24 :: MonadIO m => m (Either Text (Crypto.MnemonicSentence 24))
-- genMnemonic24 = liftIO $ bimap tshow Crypto.entropyToWords . Crypto.toEntropy @256
--   -- This size must be a 1/8th the size of the 'toEntropy' size: 256 / 8 = 32
--   <$> Crypto.Random.Entropy.getEntropy @ByteString 32

-- for recovery
phraseToSeed :: MnemonicPhrase -> Crypto.Seed
phraseToSeed (MnemonicPhrase lst) =
  let phraseMap = wordsToPhraseMap lst
      Right phrase = Crypto.mnemonicPhrase @12 $ textTo <$> Map.elems phraseMap
      Right sentence = Crypto.mnemonicPhraseToMnemonicSentence Crypto.english phrase
  in sentenceToSeed sentence

phraseToEitherSeed :: MnemonicPhrase -> Either String Crypto.Seed
phraseToEitherSeed (MnemonicPhrase lst) = do
  let phraseMap = wordsToPhraseMap lst
  phrase <- first show $ Crypto.mnemonicPhrase @12 $ textTo <$> Map.elems phraseMap
  sentence <- first show $ Crypto.mnemonicPhraseToMnemonicSentence Crypto.english phrase
  pure $ sentenceToSeed sentence

-- for generation
sentenceToSeed :: Crypto.ValidMnemonicSentence mw => Crypto.MnemonicSentence mw -> Crypto.Seed
sentenceToSeed s = Crypto.sentenceToSeed s Crypto.english ""

-- |Takes a n-sentence crypto seed and a password, and produces an encrypted key that can be
-- unlocked with the password
-- TODO: enter password 2x, to confirm
seedToRoot :: Crypto.Seed -> Text -> Crypto.XPrv
seedToRoot seed password = Crypto.generate seed (T.encodeUtf8 password)

-- | Convenience function for unpacking byte array things into 'Text'
newtype WordKey = WordKey { _unWordKey :: Int }
  deriving (Show, Eq, Ord, Enum)

wordsToPhraseMap :: [Text] -> Map.Map WordKey Text
wordsToPhraseMap = Map.fromList . zip [WordKey 1 ..]

data KadenaKey
  = HDRoot Crypto.XPrv (Maybe Text)
  | PlainKeyPair ED25519.SecretKey ED25519.PublicKey

data KeyPairYaml = KeyPairYaml
  { kpyPublic :: Text
  , kpySecret :: Text
  } deriving (Eq,Ord,Show,Read)

instance FromJSON KeyPairYaml where
  parseJSON = withObject "KeyPairYaml" $ \o -> do
    pubText <- o .: "public"
    secText <- o .: "secret"
    pure $ KeyPairYaml pubText secText

readKadenaKey :: Handle -> IO (Either String KadenaKey)
readKadenaKey h = do
  t <- T.strip <$> T.hGetContents h
  case YA.decode1Strict $ T.encodeUtf8 t of
    Left _ -> do
      case mkMnemonicPhrase $ T.words t of
        Nothing -> pure $ Left "not a valid mnemonic phrase"
        Just phrase -> do
           case phraseToEitherSeed phrase of
             Left _ -> pure $ Left "failed converting phrase to seed"
             Right seed -> pure $ Right $ HDRoot (seedToRoot seed "") Nothing
    Right (String s) -> do
      case Crypto.xprv =<< fmapL T.unpack (B16.decodeBase16 (T.encodeUtf8 s)) of
        Left _ -> pure $ Left "Could not decode HD key"
        Right xprv -> do
          hSetBuffering stderr NoBuffering
          hPutStr stderr "Enter password to decrypt key: "
          pass <- T.pack <$> withoutInputEcho getLine
          hPutStrLn stderr ""
          pure $ Right $ HDRoot xprv (Just pass)
    Right v@(Object _) -> case fromJSON v of
      Error _ -> pure $ Left "Object is not valid key material"
      Success kpy -> do
        let mres = do
              pub <- maybeCryptoError . ED25519.publicKey =<< hush (fromB16 $ kpyPublic kpy)
              sec <- maybeCryptoError . ED25519.secretKey =<< hush (fromB16 $ kpySecret kpy)
              pure $ PlainKeyPair sec pub
        pure $ note "not a valid ED25519 key pair" mres
    Right _ -> pure $ Left "Invalid JSON type for key material"

genPairFromPhrase :: MnemonicPhrase -> KeyIndex -> (EncryptedPrivateKey, PublicKey)
genPairFromPhrase phrase idx =
  generateCryptoPairFromRoot (mnemonicToRoot phrase) "" idx

newtype PublicKey = PublicKey ByteString
  deriving (Show, Eq)

newtype EncryptedPrivateKey =
  EncryptedPrivateKey { unEncryptePrivateKey :: Crypto.XPrv }

newtype Signature = Signature Crypto.XSignature
  deriving (Show, Eq)

sigToText :: Signature -> Text
sigToText (Signature sig) = toB16 $ Crypto.unXSignature sig

toSignature :: ByteString -> Either String Signature
toSignature = fmap Signature . Crypto.xsignature

pubKeyToText :: PublicKey -> Text
pubKeyToText (PublicKey pub) = toB16 pub

--TODO -- YUCK
toPubKey :: Text -> Either Text PublicKey
toPubKey txt = do
  bs <- fromB16 txt
  case BS.length bs /= 64 of
    False -> Left "PublicKey should be 64 hex characters"
    True -> pure $ PublicKey bs

encryptedPrivateKeyToText :: EncryptedPrivateKey -> Text
encryptedPrivateKeyToText (EncryptedPrivateKey xprv) = toB16 $ Crypto.unXPrv xprv

sign :: ED25519.SecretKey -> ByteString -> ED25519.Signature
sign secret msg =
  ED25519.sign secret (ED25519.toPublic secret) msg

signHD :: EncryptedPrivateKey -> Text -> ByteString -> Signature
signHD (EncryptedPrivateKey xprv) pass msg =
  Signature $ Crypto.sign @ByteString (T.encodeUtf8 pass) xprv msg

verify :: PublicKey -> Signature -> ByteString -> Bool
verify (PublicKey pub) (Signature sig) msg = Crypto.verify xpub msg sig
  where
    dummyChainCode = BS.replicate 32 minBound
    Right xpub = Crypto.xpub $ pub <> dummyChainCode

baToText :: ByteArrayAccess b => b -> Text
baToText = T.decodeUtf8 . BA.pack . BA.unpack

textTo :: IsString a => Text -> a
textTo = fromString . T.unpack

toB16 :: ByteString -> Text
toB16 = B16.encodeBase16

fromB16 :: Text -> Either Text ByteString
fromB16 txt = B16.decodeBase16 $ T.encodeUtf8 txt

readNatural :: String -> Maybe Natural
readNatural = readMaybe

fileOrStdin :: FilePath -> IO Handle
fileOrStdin fp =
  case fp of
    "-" -> pure stdin
    _ -> openFile fp ReadMode
