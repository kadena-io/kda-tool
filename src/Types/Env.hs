{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Env where

------------------------------------------------------------------------------
import           Control.Lens (makeClassy)
import           Control.Monad.Reader
import           Data.Aeson hiding (Encoding)
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import           Katip
import           Network.HTTP.Client (Manager)
import           Options.Applicative
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
  , _env_configData :: ConfigData
  , _env_rand :: GenIO
  }

makeClassy ''Env

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
  , _signArgs_encoding :: Encoding
  } deriving (Eq,Ord,Show,Read)

encodingOption :: Parser Encoding
encodingOption = option (maybeReader $ textToEncoding . T.pack)
                        (short 'e' <> long "encoding" <> value Yaml <> help "Message encoding (raw, b16, b64, b64url, or yaml (default: yaml))")

signP :: Parser SignArgs
signP = SignArgs
    <$> strOption
          (long "keyfile" <> short 'k' <>
           help "File containing plain key pair or HD key recovery phrase to sign with")
    <*> optional (option keyIndexReader
          (long "index" <> short 'i' <>
           help "Index of the HD key to sign with"))
    <*> many (strArgument (help "File(s) containing command(s) to send"))
    <*> encodingOption
  where
    keyIndexReader = maybeReader (fmap KeyIndex . readNatural)

data NodeCmdArgs = NodeCmdArgs
  { _nodeCmdArgs_files :: [FilePath]
  , _nodeCmdArgs_node :: HostPort
  } deriving (Eq,Ord,Show,Read)

nodeCmdP :: Parser NodeCmdArgs
nodeCmdP = NodeCmdArgs
  <$> many (strArgument (help "File(s) containing command(s) to send"))
  <*> option (eitherReader (hostPortFromText . T.pack))
             (long "node" <> short 'n' <>
              help "Node hostname and optional port separated by a ':'")

data Command
  = Batch [FilePath]
  | Quicksign
  | Keygen KeyType
  | Local NodeCmdArgs
  | Poll NodeCmdArgs
  | Send NodeCmdArgs
  | Sign SignArgs
  deriving (Eq,Ord,Show,Read)

data Args = Args
  { _args_command :: Command
  , _args_severity :: Severity
  , _args_configFile :: Maybe FilePath
  }

fromStr :: String -> LogStr
fromStr = logStr

envP :: Parser Args
envP = Args
  <$> commands
  <*> option (maybeReader (textToSeverity . T.pack)) (long "log-level" <> value InfoS <> help "Minimum severity to log")
  <*> optional (strOption (long "config" <> short 'c' <> help "Path of config file"))

fileArg :: Parser FilePath
fileArg = strArgument (metavar "FILE")

keyTypeP :: Parser KeyType
keyTypeP = argument (eitherReader (keyTypeFromText . T.pack)) $ mconcat
  [ completeWith (map rdr [minBound..maxBound])
  , help "Key type (plain or hd)"
  , metavar "KEY_TYPE"
  ]
  where
    rdr = T.unpack . keyTypeToText

commands :: Parser Command
commands = hsubparser
  (  command "local" (info (Local <$> nodeCmdP)
       (progDesc "Test commands locally with a node's /local endpoint"))
  <> command "poll" (info (Poll <$> nodeCmdP)
       (progDesc "Poll command results with a node's /poll endpoint"))
  <> command "send" (info (Send <$> nodeCmdP)
       (progDesc "Send commands to a node's /send endpoint"))
  <> command "sign" (info (Sign <$> signP)
       (progDesc "Sign transactions"))
  <> command "keygen" (info (Keygen <$> keyTypeP)
       (progDesc "Generate keys / recovery phrases and print them to stdout"))
--  <> command "batch" (info (Batch <$> many fileArg)
--       (progDesc "Batch multiple command files into a group"))
  )
