{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.HostPort where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           GHC.Generics
------------------------------------------------------------------------------

data HostPort = HostPort
  { _hp_host :: Text
  , _hp_port :: Maybe Int
  } deriving (Eq,Ord,Show,Read,Generic)

hostPortToText :: HostPort -> Text
hostPortToText hp = _hp_host hp <> maybe "" (\p -> ":" <> T.pack (show p)) (_hp_port hp)

hostPortFromText :: Text -> Either String HostPort
hostPortFromText t = do
    if T.null ptext
      then Right $ HostPort h Nothing
      else do
        (p,rest) <- decimal $ T.tail ptext
        if T.null rest
          then pure $ HostPort h (Just p)
          else Left "HostPort encountered invalid port"
  where
    (h,ptext) = T.break (== ':') t

instance FromJSON HostPort where
  parseJSON = withText "HostPort" $ \v -> either fail pure $ hostPortFromText v
