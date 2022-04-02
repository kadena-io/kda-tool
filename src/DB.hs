{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module DB
  ( Db(..)
  , database
  , initializeTables
  , saveRow
  ) where

------------------------------------------------------------------------------
import qualified Data.Pool as P
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple
import           Database.Beam.Postgres (Connection, Postgres, runBeamPostgres)
import           Database.Beam.Postgres.Migrate (migrationBackend)
------------------------------------------------------------------------------
import           DB.Message
------------------------------------------------------------------------------

data Db f = Db
  { _db_messages :: f (TableEntity MessageT)
  }
  deriving stock (Generic)
  deriving anyclass (Database be)

dropDatatypeName :: Text -> Text
dropDatatypeName = T.takeWhileEnd (/= '_')

migratableDb :: CheckedDatabaseSettings Postgres Db
migratableDb = defaultMigratableDbSettings `withDbModification` dbModification
  { _db_messages = modifyCheckedTable dropDatatypeName checkedTableModification
    { _msg_id = "id"
    , _msg_time = "time"
    , _msg_contents = "contents"
    }
  }

database :: DatabaseSettings Postgres Db
database = unCheckDatabase migratableDb

-- | Create the DB tables if necessary.
initializeTables :: Connection -> IO ()
initializeTables conn = runBeamPostgres conn $
  verifySchema migrationBackend migratableDb >>= \case
    VerificationFailed _ -> autoMigrate migrationBackend migratableDb
    VerificationSucceeded -> pure ()

saveRow
  :: Beamable table
  => P.Pool Connection
  -> DatabaseEntity Postgres db (TableEntity table)
  -> SqlInsertValues Postgres (table (QExpr Postgres s))
  -> IO ()
saveRow pool table i = P.withResource pool $ \c -> runBeamPostgres c $
  runInsert $ insert table i
