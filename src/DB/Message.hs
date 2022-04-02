{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DB.Message where

------------------------------------------------------------------------------
import Data.Int
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Beam
import Database.Beam.Backend.SQL.Types
------------------------------------------------------------------------------

------------------------------------------------------------------------------
data MessageT f = Message
  { _msg_id :: C f (SqlSerial Int32)
  , _msg_time :: C f UTCTime
  , _msg_contents :: C f Text }
  deriving stock (Generic)
  deriving anyclass (Beamable)

Message
  (LensFor msg_id)
  (LensFor msg_time)
  (LensFor msg_contents)
  = tableLenses

type Message = MessageT Identity
type MessageId = PrimaryKey MessageT Identity

deriving instance Eq (PrimaryKey MessageT Identity)
deriving instance Ord (PrimaryKey MessageT Identity)
deriving instance Show (PrimaryKey MessageT Identity)

deriving instance Eq Message
deriving instance Ord Message
deriving instance Show Message

-- deriving instance Eq (PrimaryKey MessageT Maybe)
-- deriving instance Ord (PrimaryKey MessageT Maybe)
-- deriving instance Show (PrimaryKey MessageT Maybe)

-- instance ToJSON (PrimaryKey MessageT Identity) where
--     toEncoding = genericToEncoding defaultOptions

-- instance FromJSON (PrimaryKey MessageT Identity)

-- instance ToJSON (PrimaryKey MessageT Maybe) where
--     toEncoding = genericToEncoding defaultOptions

-- instance FromJSON (PrimaryKey MessageT Maybe)

-- instance ToJSON (MessageT Identity) where
--     toEncoding = genericToEncoding defaultOptions

-- instance FromJSON (MessageT Identity)

-- instance ToJSON (MessageT Maybe) where
--     toEncoding = genericToEncoding defaultOptions

-- instance FromJSON (MessageT Maybe)

instance Table MessageT where
  data PrimaryKey MessageT f = MessageId (C f (SqlSerial Int32))
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = MessageId . _msg_id

unMessageId (MessageId a) = a
