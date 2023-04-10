{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Utils where

------------------------------------------------------------------------------
import           Chainweb.Api.ChainwebMeta
import           Chainweb.Api.PactCommand
import           Chainweb.Api.Sig
import           Chainweb.Api.Transaction
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as A
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import           Data.Bifunctor
import           Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.List
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Scientific
import           Data.Time
import qualified Data.YAML as Y
import qualified Data.YAML.Aeson as YA
import qualified Data.YAML.Event as Y
import qualified Data.YAML.Schema as Y
import qualified Data.YAML.Token as Y
import           Kadena.SigningTypes
import           GHC.Generics
import           Options.Applicative hiding (Parser)
import           Pact.Types.Command
import           System.Directory
import           System.FilePath
------------------------------------------------------------------------------

tshow :: Show a => a -> Text
tshow = T.pack . show

bshow :: Show a => a -> B.ByteString
bshow = T.encodeUtf8 . tshow

niceTime :: UTCTime -> String
niceTime = formatTime defaultTimeLocale "%F %T"

maybeToParser :: String -> Maybe a -> Parser a
maybeToParser nm = maybe (fail $ "Could not parse " <> nm) pure

eitherToParser :: Either String a -> Parser a
eitherToParser = either (\s -> fail $ "Could not parse: " <> s) pure

parseScientificText :: Text -> Parser Scientific
parseScientificText
    = either fail pure
    . A.parseOnly (A.scientific <* A.endOfInput)
    . T.encodeUtf8

scientificToText :: Scientific -> Text
scientificToText = LT.toStrict . toLazyText . formatScientificBuilder Fixed Nothing

lensyToJSON
  :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
lensyToJSON = genericToJSON lensyOptions

lensyParseJSON
  :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
lensyParseJSON = genericParseJSON lensyOptions

lensyOptions :: Options
lensyOptions = defaultOptions { fieldLabelModifier = lensyConstructorToNiceJson }

lensyConstructorToNiceJson :: String -> String
lensyConstructorToNiceJson fieldName = dropWhile (=='_') $ dropWhile (/='_') $ dropWhile (=='_') fieldName

newtype MaybeBatch a = MaybeBatch { unMaybeBatch :: [a] }
  deriving (Eq,Ord,Show,Functor)

instance FromJSON a => FromJSON (MaybeBatch a) where
  parseJSON v =
    case v of
      Object o -> do
        mcmds <- o .:? "cmds"
        case mcmds of
          Nothing -> MaybeBatch . (:[]) <$> parseJSON v
          Just cs -> pure $ MaybeBatch cs
      _ -> MaybeBatch . (:[]) <$> parseJSON v

--parseAsJsonOrYaml :: FromJSON a => [LB.ByteString] -> Either [String] [a]
--parseAsJsonOrYaml bss =
--  case partitionEithers $ A.eitherDecode <$> bss of
--    ([],lvs) -> Right $ concat $ map unMaybeBatch lvs
--    (esJ,_) -> case partitionEithers $ YA.decode1Strict . LB.toStrict <$> bss of
--      ([],rvs) -> Right $ concat $ map unMaybeBatch rvs
--      (esY,_) -> Left (map show esJ ++ map show esY)

--parseJsonOrYamlCommand :: [LB.ByteString] -> Either [String] [Transaction]
--parseJsonOrYamlCommand bss =
parseAsJsonOrYaml :: Bool -> [LB.ByteString] -> Either [String] [Transaction]
parseAsJsonOrYaml requireSigs bss =
  case partitionEithers $ A.eitherDecode <$> bss of
    ([],lvs) -> Right $ concat $ map unMaybeBatch lvs
    (esJ,_) -> case partitionEithers $ parseTransactionViaSigData requireSigs <$> bss of
      ([],rvs) -> Right $ concat $ map unMaybeBatch rvs
      (esY,_) -> Left (map show esJ ++ map show esY)

parseTransactionViaSigData :: Bool -> LB.ByteString -> Either String (MaybeBatch Transaction)
parseTransactionViaSigData requireSigs bs = do
  MaybeBatch sds <- first show $ YA.decode1Strict $ LB.toStrict bs
  fmap MaybeBatch $ sequence $ map (commandSigDataToTransaction requireSigs) sds

-- | Converts Pact's 'SigData' type to chainweb-api's 'Transaction'
-- TODO SigData overlaps with types in the signing API as well. At some point we
-- need to restructure all this into a much more holistic set of types.
--
-- We're parsing through CommandSigData now because that's the signing API type
-- and it is more flexible than the old Pact SigData type because it should
-- allow the hash field to be either present or absent.
--sigDataToTransaction :: Bool -> SigData Text -> Either String Transaction
--sigDataToTransaction requireSigs sd = do
--    cmdText <- note "Error: SigData 'cmd' field not found" $ _sigDataCmd sd
--    pc <- eitherDecodeStrict $ T.encodeUtf8 cmdText
--    sigs <- note "Error: SigData has missing signatures" $ sequence $ map (addDummy . snd) $ _sigDataSigs sd
--    pure $ mkTransaction pc (map userSigToSig sigs)
--  where
--    addDummy = maybe (if requireSigs then Nothing else Just dummySig) Just
--    dummySig = UserSig "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

-- | Converts a 'CommandSigData' type to chainweb-api's 'Transaction'
commandSigDataToTransaction :: Bool -> CommandSigData -> Either String Transaction
commandSigDataToTransaction requireSigs csd = do
    let cmdText = _csd_cmd csd
    pc <- eitherDecodeStrict $ T.encodeUtf8 cmdText
    sigs <- note "Error: CommandSigData has missing signatures" $
      sequence $ map (addDummy . _s_userSig) $ unSignatureList $ _csd_sigs csd
    pure $ mkTransaction pc (map userSigToSig sigs)
  where
    addDummy = maybe (if requireSigs then Nothing else Just dummySig) Just
    dummySig = UserSig "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

-- | Converts chainweb-api's 'Sig' type to Pact's 'UserSig'.
userSigToSig :: UserSig -> Sig
userSigToSig = Sig . _usSig

-- | Converts Pact's 'UserSig' type to chainweb-api's 'Sig'.
sigToUserSig :: Sig -> UserSig
sigToUserSig = UserSig . unSig

--data SigData a = SigData
--  { _sigDataHash :: PactHash
--  , _sigDataSigs :: [(PublicKeyHex, Maybe UserSig)]
--  -- ^ This is not a map because the order must be the same as the signers inside the command.
--  , _sigDataCmd :: Maybe a
--  } deriving (Eq,Show,Generic)
--mkTransaction :: PactCommand -> [Sig] -> Transaction
--mkTransaction pc sigs =
--    Transaction (Hash h) sigs pc (decodeUtf8 cmdBytes)
--  where
--    cmdBytes = BL.toStrict $ encode pc
--
--    -- This function only returns Left when one of the first two args is
--    -- invalid. In this case we're supplying them both as constants, so the
--    -- incomplete pattern match is safe here.
--    Right h = blake2b 32 mempty cmdBytes

niceQuoteEncodeYaml :: ToJSON a => a -> LB.ByteString
niceQuoteEncodeYaml a = YA.encodeValue' senc Y.UTF8 [toJSON a]
  where
    encScalar s@(Y.SStr t) = case T.find (== '"') t of
      Just _ -> Right (Y.untagged, Y.SingleQuoted, t)
      Nothing -> Y.schemaEncoderScalar Y.coreSchemaEncoder s
    encScalar s = Y.schemaEncoderScalar Y.coreSchemaEncoder s
    senc = Y.setScalarStyle encScalar Y.coreSchemaEncoder

-- | Saves a CommandSigData as JSON if it is fully signed or as YAML if more signatures are needed.
saveCommandSigData
  :: FilePath
  -- ^ The filename without the extension (.yaml or .json gets added on
  -- depending on whether the command is fully signed)
  -> CommandSigData
  -> IO FilePath
saveCommandSigData fname csd = do
  case filter (isNothing . _s_userSig) $ unSignatureList $ _csd_sigs csd of
    [] -> do
      case commandSigDataToCommand csd of
        Left _ -> writeYaml fname csd
        Right c -> writeJson fname c
    _ -> writeYaml fname csd

writeYaml :: FilePath -> CommandSigData -> IO FilePath
writeYaml fname csd = do
  let fp = fname <> ".yaml"
  LB.writeFile fp $ niceQuoteEncodeYaml csd
  pure fp

writeJson :: FilePath -> Command Text -> IO FilePath
writeJson fname c = do
  let fp = fname <> ".json"
  LB.writeFile fp $ encode c
  pure fp

hasYamlExtension :: FilePath -> Bool
hasYamlExtension fp =
  T.isSuffixOf ".yaml" (T.pack fp) ||
  T.isSuffixOf ".json" (T.pack fp)

countSigs :: CommandSigData -> Int
countSigs = length . filter (isJust . _s_userSig) . unSignatureList . _csd_sigs

txChain :: Transaction -> Text
txChain = _chainwebMeta_chainId . _pactCommand_meta . _transaction_cmd

txNetwork :: Transaction -> Maybe Text
txNetwork = _pactCommand_network . _transaction_cmd


data PathCompleterOpts = PathCompleterOpts
    { pcoAbsolute :: Bool
    , pcoRelative :: Bool
    , pcoRootDir :: Maybe FilePath
    , pcoFileFilter :: FilePath -> Bool
    , pcoDirFilter :: FilePath -> Bool
    }

defaultPathCompleterOpts :: PathCompleterOpts
defaultPathCompleterOpts = PathCompleterOpts
    { pcoAbsolute = True
    , pcoRelative = True
    , pcoRootDir = Nothing
    , pcoFileFilter = const True
    , pcoDirFilter = const True
    }

fileCompleter :: Completer
fileCompleter = pathCompleterWith defaultPathCompleterOpts

fileExtCompleter :: [String] -> Completer
fileExtCompleter exts = pathCompleterWith defaultPathCompleterOpts { pcoFileFilter = (`elem` exts) . takeExtension }

dirCompleter :: Completer
dirCompleter = pathCompleterWith defaultPathCompleterOpts { pcoFileFilter = const False }

pathCompleterWith :: PathCompleterOpts -> Completer
pathCompleterWith PathCompleterOpts {..} = mkCompleter $ \inputRaw -> do
    -- Unescape input, to handle single and double quotes. Note that the
    -- results do not need to be re-escaped, due to some fiddly bash
    -- magic.
    let input = unescapeBashArg inputRaw
    let (inputSearchDir0, searchPrefix) = splitFileName input
        inputSearchDir = if inputSearchDir0 == "./" then "" else inputSearchDir0
    msearchDir <-
        case (isRelative inputSearchDir, pcoAbsolute, pcoRelative) of
            (True, _, True) -> do
                if "~" `isPrefixOf` inputSearchDir
                  then do
                    searchDir <- tildeExpand inputSearchDir
                    pure $ Just searchDir
                  else do
                    rootDir <- maybe getCurrentDirectory return pcoRootDir
                    return $ Just (rootDir </> inputSearchDir)
            (False, True, _) -> return $ Just inputSearchDir
            _ -> return Nothing
    case msearchDir of
        Nothing
            | input == "" && pcoAbsolute -> return ["/"]
            | otherwise -> return []
        Just searchDir -> do
            entries <- getDirectoryContents searchDir `catch` \(_ :: IOException) -> return []
            results <- fmap catMaybes $ forM entries $ \entry ->
                -- Skip . and .. unless user is typing . or ..
                if entry `elem` ["..", "."] && searchPrefix `notElem` ["..", "."] then return Nothing else
                    if searchPrefix `isPrefixOf` entry
                        then do
                            let path = searchDir </> entry
                            case (pcoFileFilter path, pcoDirFilter path) of
                                (True, True) -> return $ Just $ escapeShellArg (inputSearchDir </> entry)
                                (fileAllowed, dirAllowed) -> do
                                    isDir <- doesDirectoryExist path
                                    if (if isDir then dirAllowed else fileAllowed)
                                        then return $ Just $ escapeShellArg (inputSearchDir </> entry)
                                        else return Nothing
                        else return Nothing
            return results

escapeShellArg :: String -> String
escapeShellArg s = go s
  where
    go [] = []
    go (c:rest) = (if shouldEscape c then ('\\' :) else id) $ c : go rest

shouldEscape :: Char -> Bool
shouldEscape c = not (isAlphaNum c) && case c of
  ',' -> False
  '.' -> False
  '_' -> False
  '+' -> False
  ':' -> False
  '@' -> False
  '%' -> False
  '/' -> False
  '-' -> False
  '~' -> False
  _ -> True

tildeExpand :: String -> IO String
tildeExpand s = case s of
  ('~' : rest) -> do
    let (user,suffix) = span (/= '/') rest
    tildeDir <- case user of
      "" -> getHomeDirectory
      _ -> do
        h <- getHomeDirectory
        pure $ dropFileName h </> user
    pure (tildeDir <> suffix)
  _ -> pure s

unescapeBashArg :: String -> String
unescapeBashArg ('\'' : rest) = rest
unescapeBashArg ('\"' : rest) = go rest
  where
    go [] = []
    go ('\\' : x : xs)
        | x `elem` ("$`\"\\\n" :: String) = x : xs
        | otherwise = '\\' : x : go xs
    go (x : xs) = x : go xs
unescapeBashArg input = go input
  where
    go [] = []
    go ('\\' : x : xs) = x : go xs
    go (x : xs) = x : go xs
