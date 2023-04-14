{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Env where

------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.Transaction
import           Control.Error
import           Control.Lens (makeLenses)
import           Control.Monad.Reader
import           Data.Aeson hiding (Encoding)
import           Data.Binary.Builder
import qualified Data.ByteString as B
import           Data.Default
import           Data.Function
import           Data.List
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Ord
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Katip
import           Network.HTTP.Client
import           Options.Applicative
import           Options.Applicative.Help.Pretty hiding ((</>))
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Info
import           System.Random.MWC
import           Text.Printf
------------------------------------------------------------------------------
import           Keys
import           Types.Encoding
import           Types.HostPort
import           Types.KeyType
import           Utils
------------------------------------------------------------------------------

data ConfigData = ConfigData
  { _configData_networks :: Maybe (Map Text HostPort)
  , _configData_txRepos :: Maybe [Text]
  } deriving (Eq,Ord,Show,Read)

instance Default ConfigData where
  def = ConfigData mempty mempty

instance FromJSON ConfigData where
  parseJSON = withObject "ConfigData" $ \v -> ConfigData
    <$> v .: "networks"
    <*> v .:? "tx-repos"

lookupConfiguredNetwork :: Env -> Text -> Either String HostPort
lookupConfiguredNetwork e net = do
  m <- note "No networks configured" $ _configData_networks $ _env_configData e
  note ("\"" <> T.unpack net <> "\" network does not exist") $ M.lookup net m

groupByNetwork :: Env -> [Transaction] -> Either String [(HostPort, [Transaction])]
groupByNetwork e allTxs = do
    hps <- case partitionEithers ehps of
      ([], hps)  -> Right hps
      (es, _)  -> do
        Left $ unlines $
          "Some of your transactions don't specify a network or specify a network that is not configured.  To resolve this you can either add missing networks to your transactions,  or specify a node with -n."
          : es
    Right (zip hps $ map NE.toList networkGroups)
  where
    networkGroups = NE.groupBy ((==) `on` txNetwork) $ sortBy (comparing txNetwork) allTxs
    ehps = map (lookupConfiguredNetwork e <=<
                note "Transaction does not specify a network" .
                txNetwork .
                NE.head) networkGroups

handleOptionalNode
  :: Monad m
  => Env
  -> [Transaction]
  -> Maybe HostPort
  -> ExceptT String m [(HostPort, [Transaction])]
handleOptionalNode e allTxs Nothing = hoistEither $ groupByNetwork e allTxs
handleOptionalNode _ allTxs (Just hp) = pure [(hp, allTxs)]

data Env = Env
  { _env_httpManager :: Manager
  , _env_logEnv :: LogEnv
  , _env_configData :: ConfigData
  , _env_rand :: GenIO
  }

makeLenses ''Env

--instance (MonadIO m, MonadReader Env m) => Katip m where
--  getLogEnv = asks _env_logEnv
--  localLogEnv f = local (over env_logEnv f)

logEnv :: MonadIO m => Env -> Severity -> LogStr -> m ()
logEnv e = logLE (_env_logEnv e)

logLE :: MonadIO m => LogEnv -> Severity -> LogStr -> m ()
logLE le sev s = runKatipT le $ logMsg mempty sev s

logFLE :: (MonadIO m, LogItem a) => LogEnv -> Severity -> a -> LogStr -> m ()
logFLE le sev a msg = runKatipT le $ logF a mempty sev msg

instance ToObject Request where
  toObject r = mconcat
    [ "method" .= (decodeUtf8 $ method r)
    , "url" .= reqUrl r
    , "headers" .= tshow (requestHeaders r)
    , "body" .= bodyText (requestBody r)
    ]

bodyText :: RequestBody -> Text
bodyText r = case r of
  RequestBodyLBS lbs -> decodeUtf8 $ toS lbs
  RequestBodyBS bs -> toS bs
  RequestBodyBuilder _ b -> toS $ toLazyByteString b
  _ -> "body not available"

reqUrl :: Request -> Text
reqUrl r =
    T.pack $ printf "%s://%s:%d%s%s" s h po pa qs
  where
    s = if secure r then "https" else "http" :: String
    h = decodeUtf8 $ host r
    po = port r
    pa = decodeUtf8 $ path r
    qs = let q = queryString r
          in if B.length q == 0 then "" else "?" <> decodeUtf8 q

instance LogItem Request where
  payloadKeys v _ = case v of
    V0 -> SomeKeys []
    V1 -> SomeKeys ["method"]
    V2 -> SomeKeys ["url"]
    V3 -> AllKeys

data ChainweaverFile = ChainweaverFile
  deriving (Eq,Ord,Show,Read)

data SignArgs = SignArgs
  { _signArgs_keyFile :: Either FilePath ChainweaverFile
  , _signArgs_keyInd :: Maybe KeyIndex
  , _signArgs_files :: [FilePath]
  } deriving (Eq,Ord,Show,Read)

data WalletSignMethod = OldSign | Quicksign
  deriving (Eq,Ord,Show,Read)

data WalletSignArgs = WalletSignArgs
  { _walletSignArgs_files :: [FilePath]
  , _walletSignArgs_method :: WalletSignMethod
  } deriving (Eq,Ord,Show,Read)

encodingOption :: Parser Encoding
encodingOption = option (maybeReader $ textToEncoding . T.pack) $ mconcat
  [ short 'e'
  , long "encoding"
  , value Yaml
  , metavar "ENCODING"
  , help "Message encoding (raw, b16, b64, b64url, or yaml (default: yaml))"
  , completeWith (map (T.unpack . encodingToText) [minBound..maxBound])
  ]

keyIndexP :: Parser KeyIndex
keyIndexP = option keyIndexReader $ mconcat
    [ long "index"
    , short 'i'
    , help "Index of the HD key to sign with"
    , metavar "KEY_INDEX"
    ]
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)

txFileP :: Parser FilePath
txFileP = strArgument $ mconcat
  [ help "YAML file(s) containing transactions"
  , metavar "TX_FILE"
  , completer $ fileExtCompleter [".yaml", ".json"]
  ]

keyFileP :: Parser FilePath
keyFileP = strOption $ mconcat
  [ long "keyfile"
  , short 'k'
  , metavar "KEY_FILE"
  , help "File containing plain key pair or HD key recovery phrase to sign with"
  , completer $ fileExtCompleter [".kda", ".phrase"]
  ]

getChainweaverDir :: IO FilePath
getChainweaverDir = do
  home <- getHomeDirectory
  pure $ case os of
    "darwin" -> home </> "Library" </> "Application Support" </> "io.kadena.chainweaver"
    _ -> home </> ".local" </> "share" </> "chainweaver"

getKeyFile :: Either FilePath ChainweaverFile -> IO (FilePath, Handle)
getKeyFile (Left fp) = do
  let fpStr = case fp of
        "-" -> "stdin"
        _ -> fp
  h <- fileOrStdin fp
  pure (fpStr, h)
getKeyFile (Right ChainweaverFile) = do
  d <- getChainweaverDir
  let fp = d </> "BIPStorage_RootKey"
  h <- openFile fp ReadMode
  pure (fp, h)

keyFileOrChainweaverP :: Parser (Either FilePath ChainweaverFile)
keyFileOrChainweaverP = flag' (Right ChainweaverFile) opts <|> fmap Left keyFileP
  where
    opts = mconcat
      [ long "chainweaver"
      , help "Sign using key stored by desktop Chainweaver on your filesystem"
      ]

signP :: Parser SignArgs
signP = SignArgs <$> keyFileOrChainweaverP <*> optional keyIndexP <*> many txFileP

walletMethodP :: Parser WalletSignMethod
walletMethodP = flag Quicksign OldSign $ mconcat
  [ long "old"
  , help "Sign with old single-tx wallet signing API"
  ]

walletSignP :: Parser WalletSignArgs
walletSignP = WalletSignArgs <$> many txFileP <*> walletMethodP

data NodeTxCmdArgs = NodeTxCmdArgs
  { _nodeTxCmdArgs_files :: [FilePath]
  , _nodeTxCmdArgs_node :: Maybe HostPort
  } deriving (Eq,Ord,Show,Read)

nodeOptP :: Parser HostPort
nodeOptP = option (eitherReader (hostPortFromText . T.pack)) $ mconcat
  [ long "node"
  , short 'n'
  , metavar "NODE"
  , help "Node hostname and optional port separated by a ':'"
  ]

nodeArgP :: Parser HostPort
nodeArgP = argument (eitherReader (hostPortFromText . T.pack)) $ mconcat
  [ metavar "NODE"
  , help "Node hostname and optional port separated by a ':'"
  ]

nodeTxCmdP :: Parser NodeTxCmdArgs
nodeTxCmdP = NodeTxCmdArgs <$> many txFileP <*> optional nodeOptP

data Holes = Holes
  deriving (Eq,Ord,Show,Read)

data TxArgs = TxArgs
  { _txArgs_templateName :: Text
  } deriving (Eq,Ord,Show,Read)

data GitHubTemplate = GitHubTemplate
  { _ght_templateName :: Text
  , _ght_templateRepo :: Maybe Text
  } deriving (Eq,Ord,Show,Read)

data TemplateArg
  = TemplateFile FilePath
  | TemplateGitHub GitHubTemplate
  deriving (Eq,Ord,Show,Read)

data GenTxArgs = GenTxArgs
  { _genTxArgs_template :: TemplateArg
  , _genTxArgs_operation :: Either Holes GenData
  , _genTxArgs_old :: Bool
  } deriving (Eq,Ord,Show,Read)

data GenData = GenData
  { _genData_dataFile :: Maybe FilePath
  , _genData_outFilePat :: Maybe FilePath
  } deriving (Eq,Ord,Show,Read)

genDataP :: Parser GenData
genDataP = GenData <$> optional dataFileP <*> optional filePatP

holesP :: Parser (Either Holes GenData)
holesP = flag' (Left Holes) $ mconcat
  [ long "holes"
  , help "Display the holes to be filled in a template"
  ]

filePatP :: Parser FilePath
filePatP = strOption $ mconcat
  [ long "out-file"
  , short 'o'
  , metavar "OUT_PAT"
  , helpDoc $ Just $ mconcat
    [ text "Pattern to use for output filenames"
    , hardline
    , text "(example: \"tx-{{chain}}.yaml\")"
    ]
  ]

dataFileP :: Parser FilePath
dataFileP = strOption $ mconcat
  [ long "data"
  , short 'd'
  , help "YAML data file for filling the tx template"
  , metavar "DATA_FILE"
  , completer fileCompleter
  ]

chainP :: Parser ChainId
chainP = fmap ChainId $ argument auto $ mconcat
  [ help "Chain ID"
  , metavar "CHAIN_ID"
  ]

templateFileP :: Parser FilePath
templateFileP = strOption $ mconcat
  [ long "tfile"
  , short 't'
  , help "YAML file with a mustache transaction template"
  , metavar "TEMPLATE_FILE"
  , completer $ fileExtCompleter [".yaml", ".ktpl"]
  ]

templateNameP :: Parser Text
templateNameP = strOption $ mconcat
  [ long "gh-tmpl"
  , short 'g'
  , help "Name of a .ktpl tx template in a GitHub repo"
  , metavar "TEMPLATE_NAME"
  ]

repoP :: Parser Text
repoP = strOption $ mconcat
  [ long "repo"
  , short 'r'
  , help "Name of a GitHub repo with templates"
  , metavar "TEMPLATE_REPO"
  ]

githubTmplP :: Parser GitHubTemplate
githubTmplP = GitHubTemplate <$> templateNameP <*> optional repoP

templateArgP :: Parser TemplateArg
templateArgP = (TemplateFile <$> templateFileP)
           <|> (TemplateGitHub <$> githubTmplP)

genTxArgsP :: Parser GenTxArgs
genTxArgsP = GenTxArgs <$> templateArgP <*> (holesP <|> fmap Right genDataP) <*> oldP

oldP :: Parser Bool
oldP = flag False True $ mconcat
  [ long "old"
  , help "Output old SigData format (deprecated, will be removed)"
  ]

data SubCommand
  = CombineSigs [FilePath]
  | Cut HostPort
  | GenTx GenTxArgs
  | Keygen KeyType
  | ListKeys (Either FilePath ChainweaverFile) (Maybe KeyIndex)
  | Local NodeTxCmdArgs
  | Mempool HostPort Text ChainId
  | Poll NodeTxCmdArgs
  | Send NodeTxCmdArgs
  | Sign SignArgs
  | WalletSign WalletSignArgs
  deriving (Eq,Ord,Show)

data Args = Args
  { _args_command :: SubCommand
  , _args_severity :: Severity
  , _args_verbosity :: Verbosity
  , _args_configFile :: Maybe FilePath
  }

fromStr :: String -> LogStr
fromStr = logStr

logLevelP :: Parser Severity
logLevelP = option (maybeReader (textToSeverity . T.pack)) $ mconcat
  [ long "log-level"
  , value InfoS
  , metavar "LOG_LEVEL"
  , help "Minimum severity to log"
  , completeWith (map (T.unpack . renderSeverity) [minBound..maxBound])
  ]

verbosityP :: Parser Verbosity
verbosityP = option (maybeReader (verbosityFromText . T.pack)) $ mconcat
  [ long "verbosity"
  , short 'v'
  , metavar "NUM"
  , value V1
  , help "Log verbosity (0-3)"
  , completeWith (map show [0..3 :: Int])
  ]

verbosityFromText :: Text -> Maybe Verbosity
verbosityFromText "0" = Just V0
verbosityFromText "1" = Just V1
verbosityFromText "2" = Just V2
verbosityFromText "3" = Just V3
verbosityFromText _ = Nothing

configFileP :: Parser FilePath
configFileP = strOption $ mconcat
  [ long "config-file"
  , short 'c'
  , metavar "FILE"
  , help "JSON file with general configuration options"
  , completer $ fileExtCompleter [".json"]
  ]

envP :: Parser Args
envP = Args <$> commands <*> logLevelP <*> verbosityP <*> optional configFileP

keyTypeP :: Parser KeyType
keyTypeP = argument (eitherReader (keyTypeFromText . T.pack)) $ mconcat
  [ metavar "KEY_TYPE"
  , help "Key type (plain or hd)"
  , completeWith (map rdr [minBound..maxBound])
  ]
  where
    rdr = T.unpack . keyTypeToText

listKeysP :: Parser SubCommand
listKeysP = ListKeys <$> keyFileOrChainweaverP <*> optional indP
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)
    indP = option keyIndexReader $ mconcat
      [ long "index"
      , short 'i'
      , help "Maximum key index"
      , metavar "KEY_INDEX"
      ]

commands :: Parser SubCommand
commands =
  hsubparser templateCommands <|>
  hsubparser signingCommands <|>
  hsubparser nodeCommands <|>
  hsubparser keyCommands


templateCommands :: Mod CommandFields SubCommand
templateCommands = mconcat
  [ command "gen" (info (GenTx <$> genTxArgsP)
      (progDesc "Generate transactions from a template file"))
  , commandGroup "Transaction Templating Commands"
  ]

signingCommands :: Mod CommandFields SubCommand
signingCommands = mconcat
  [ command "combine-sigs" (info (CombineSigs <$> many txFileP)
      (progDesc "Combine signatures from multiple files"))
  , command "sign" (info (Sign <$> signP)
      (progDesc "Sign transactions"))
  , command "wallet-sign" (info (WalletSign <$> walletSignP)
      (progDesc "Send transactions to a wallet for signing"))
  , commandGroup "Transaction Signing Commands"
  , hidden
  ]

networkP :: Parser Text
networkP = strArgument $ mconcat
  [ metavar "NETWORK"
  , help "The node's network ID (i.e. mainnet01, testnet04, etc)"
  , completeWith ["mainnet01", "testnet04", "development"]
  ]

nodeCommands :: Mod CommandFields SubCommand
nodeCommands = mconcat
  [ command "local" (info (Local <$> nodeTxCmdP)
      (progDesc "Test commands locally with a node's /local endpoint"))
  , command "poll" (info (Poll <$> nodeTxCmdP)
      (progDesc "Poll command results with a node's /poll endpoint"))
  , command "send" (info (Send <$> nodeTxCmdP)
      (progDesc "Send commands to a node's /send endpoint"))
  , command "cut" (info (Cut <$> nodeArgP)
      (progDesc "Query a node's /cut endpoint"))
  , command "mempool" (info (Mempool <$> nodeArgP <*> networkP <*> chainP)
      (progDesc "Get mempool pending transactions"))
  , commandGroup "Node Interaction Commands"
  , hidden
  ]

keyCommands :: Mod CommandFields SubCommand
keyCommands = mconcat
  [ command "keygen" (info (Keygen <$> keyTypeP)
      (progDesc "Generate a key / recovery phrase and print to stdout"))
  , command "list-keys" (info listKeysP
      (progDesc "List the public keys for an HD key recovery phrase"))
  , commandGroup "Key Generation Commands"
  , hidden
  ]
