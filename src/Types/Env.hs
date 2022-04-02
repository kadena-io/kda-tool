{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Env where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Default
import           Data.Pool
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam.Postgres
import           Katip
import           Network.HTTP.Client (Manager)
import           Options.Applicative
import           System.Random.MWC
------------------------------------------------------------------------------
import           Utils
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
  , _env_dbConnPool :: Pool Connection
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

data Command = Alpha | Beta | Gamma
  deriving (Eq,Ord,Show,Read,Enum)

data Args = Args
  { _args_command :: Command
  , _args_connect :: Connect
  , _args_severity :: Severity
  , _args_configFile :: Maybe FilePath
  }

fromStr :: String -> LogStr
fromStr = logStr

data Connect = PGInfo ConnectInfo | PGString ByteString
  deriving (Eq,Show)

-- | Create a `Pool` based on `Connect` settings designated on the command line.
getPool :: IO Connection -> IO (Pool Connection)
getPool getConn = do
  caps <- getNumCapabilities
  createPool getConn close 1 5 caps

mkPool :: Connect ->  IO (Pool Connection)
mkPool (PGInfo ci) = getPool (connect ci)
mkPool _ = error "This DB mode not supported"

-- | A bracket for `Pool` interaction.
withPool :: Connect -> (Pool Connection -> IO a) -> IO a
withPool (PGInfo ci) = bracket (getPool (connect ci)) destroyAllResources
withPool (PGString s) = bracket (getPool (connectPostgreSQL s)) destroyAllResources

envP :: Parser Args
envP = Args
  <$> commands
  <*> connectP
  <*> option (maybeReader (textToSeverity . T.pack)) (long "log-level" <> value InfoS <> help "Minimum severity to log")
  <*> optional (strOption (long "config" <> short 'c' <> help "Path of config file"))

connectP :: Parser Connect
connectP = (PGString <$> pgstringP) <|> (PGInfo <$> connectInfoP)

dbdirP :: Parser FilePath
dbdirP = strOption (long "dbdir" <> help "Directory for self-run postgres")

pgstringP :: Parser ByteString
pgstringP = strOption (long "dbstring" <> help "Postgres Connection String")

-- | These defaults are pulled from the postgres-simple docs.
connectInfoP :: Parser ConnectInfo
connectInfoP = ConnectInfo
  <$> strOption   (long "dbhost" <> value "localhost" <> help "Postgres DB hostname")
  <*> option auto (long "dbport" <> value 5432        <> help "Postgres DB port")
  <*> strOption   (long "dbuser" <> value "postgres"  <> help "Postgres DB user")
  <*> strOption   (long "dbpass" <> value ""          <> help "Postgres DB password")
  <*> strOption   (long "dbname" <> help "Postgres DB name")

commands :: Parser Command
commands = hsubparser
  (  command "alpha" (info (pure Alpha)
       (progDesc "Alpha"))
  <> command "beta" (info (pure Beta)
       (progDesc "Beta"))
  <> command "gamma" (info (pure Gamma)
       (progDesc "Gamma"))
  )
