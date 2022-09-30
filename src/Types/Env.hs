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
import           Control.Lens (makeLenses)
import           Control.Monad.Reader
import           Data.Aeson hiding (Encoding)
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import           Katip
import           Network.HTTP.Client (Manager)
import           Options.Applicative
import           Options.Applicative.Help.Pretty
import           System.Random.MWC
------------------------------------------------------------------------------
import           Keys
import           Types.Encoding
import           Types.HostPort
import           Types.KeyType
------------------------------------------------------------------------------

data ConfigData = ConfigData
  { _configData_foo :: Maybe Text
  } deriving (Eq,Ord,Show,Read)

instance Default ConfigData where
  def = ConfigData Nothing

instance FromJSON ConfigData where
  parseJSON = withObject "ConfigData" $ \v -> ConfigData
    <$> v .: "foo"

data Env = Env
  { _env_httpManager :: Manager
  , _env_logEnv :: LogEnv
--  , _env_configData :: ConfigData
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

data SignArgs = SignArgs
  { _signArgs_keyFile :: FilePath
  , _signArgs_keyInd :: Maybe KeyIndex
  , _signArgs_files :: [FilePath]
  , _signArgs_quiet :: Bool
  } deriving (Eq,Ord,Show,Read)

encodingOption :: Parser Encoding
encodingOption = option (maybeReader $ textToEncoding . T.pack) $ mconcat
  [ short 'e'
  , long "encoding"
  , value Yaml
  , metavar "ENCODING"
  , help "Message encoding (raw, b16, b64, b64url, or yaml (default: yaml))"
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
  ]

keyFileP :: Parser FilePath
keyFileP = strOption $ mconcat
  [ long "keyfile"
  , short 'k'
  , metavar "KEY_FILE"
  , help "File containing plain key pair or HD key recovery phrase to sign with"
  ]

quietP :: Parser Bool
quietP = switch (long "quiet" <> short 'q' <> help "Quiet mode")

signP :: Parser SignArgs
signP = SignArgs <$> keyFileP <*> optional keyIndexP <*> many txFileP <*> quietP

data NodeTxCmdArgs = NodeTxCmdArgs
  { _nodeTxCmdArgs_files :: [FilePath]
  , _nodeTxCmdArgs_node :: HostPort
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
nodeTxCmdP = NodeTxCmdArgs <$> many txFileP <*> nodeOptP

data Holes = Holes
  deriving (Eq,Ord,Show,Read)

data GenTxArgs = GenTxArgs
  { _genTxArgs_templateFile :: FilePath
  , _genTxArgs_operation :: Either Holes GenData
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

templateFileP :: Parser FilePath
templateFileP = strArgument $ mconcat
  [ help "YAML file with a mustache transaction template"
  , metavar "TEMPLATE_FILE"
  --, long "template"
  --, short 't'
  ]

filePatP :: Parser FilePath
filePatP = strOption $ mconcat
  [ long "out-file"
  , short 'o'
  , metavar "OUT_PAT"
  --, help "Pattern to use for output filenames (ex: \"tx-{{chain}}.yaml\")"
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
  ]

chainP :: Parser ChainId
chainP = fmap ChainId $ argument auto $ mconcat
  [ help "Chain ID"
  , metavar "CHAIN_ID"
  ]

genTxArgsP :: Parser GenTxArgs
genTxArgsP = GenTxArgs <$> templateFileP <*> (holesP <|> fmap Right genDataP)

data SubCommand
  = CombineSigs [FilePath]
  | Cut HostPort
  | GenTx GenTxArgs
  | Keygen KeyType
  | ListKeys FilePath KeyIndex
  | Local NodeTxCmdArgs
  | Mempool HostPort Text ChainId
  | Poll NodeTxCmdArgs
  | Send NodeTxCmdArgs
  | Sign SignArgs
--  | Batch [FilePath]
--  | Quicksign
  deriving (Eq,Ord,Show)

data Args = Args
  { _args_command :: SubCommand
  , _args_severity :: Severity
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

envP :: Parser Args
envP = Args <$> commands <*> logLevelP

keyTypeP :: Parser KeyType
keyTypeP = argument (eitherReader (keyTypeFromText . T.pack)) $ mconcat
  [ completeWith (map rdr [minBound..maxBound])
  , help "Key type (plain or hd)"
  , metavar "KEY_TYPE"
  ]
  where
    rdr = T.unpack . keyTypeToText

listKeysP :: Parser SubCommand
listKeysP = ListKeys <$> hdKeyFileP <*> indP
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)
    hdKeyFileP = strArgument $ mconcat
      [ help "HD key file"
      , metavar "KEY_FILE"
      ]
    indP = argument keyIndexReader $ mconcat
      [ help "Maximum key index"
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
      (progDesc "Generate transactions from a template"))
  , commandGroup "Transaction Templating Commands"
  ]

signingCommands :: Mod CommandFields SubCommand
signingCommands = mconcat
  [ command "combine-sigs" (info (CombineSigs <$> many txFileP)
      (progDesc "Combine signatures from multiple files"))
  , command "sign" (info (Sign <$> signP)
      (progDesc "Sign transactions"))
  , commandGroup "Transaction Signing Commands"
  , hidden
  ]

networkP :: Parser Text
networkP = strArgument $ mconcat
  [ metavar "NETWORK"
  , help "The node's network ID (i.e. mainnet01, testnet04, etc)"
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
