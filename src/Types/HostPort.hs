{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.HostPort where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Bifunctor
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
        (p,rest) <- first (const $ T.unpack $ "Error parsing port in: "<>t) $ decimal $ T.tail ptext
        if T.null rest
          then pure $ HostPort h (Just p)
          else Left $ "Could not completely parse port in: " <> T.unpack t
  where
    (h,ptext) = T.break (== ':') t

instance FromJSON HostPort where
  parseJSON = withText "HostPort" $ \v -> either fail pure $ hostPortFromText v

data Scheme = Http | Https
  deriving (Eq,Ord,Show,Read,Generic)

schemeText :: Scheme -> Text
schemeText Https = "https"
schemeText Http = "http"

data SchemeHostPort = SchemeHostPort
  { _shp_scheme :: Maybe Scheme
  , _shp_hostPort :: HostPort
  } deriving (Eq,Ord,Show,Read,Generic)

schemeHostPortFromText :: Text -> Either String SchemeHostPort
schemeHostPortFromText t = do
    (mscheme, rest) <-
      case T.breakOn "://" t of
        (_,"") -> Right (Nothing, t)
        (s,hp) -> do
          scheme <- case s of
            "http" -> Right Http
            "https" -> Right Https
            _ -> Left $ "Invalid scheme for host " <> T.unpack t
          Right (Just scheme, T.drop 3 hp)
    hp <- hostPortFromText rest
    pure $ SchemeHostPort mscheme hp

schemeHostPortToText :: SchemeHostPort -> Text
schemeHostPortToText (SchemeHostPort mscheme hp) =
    schemePart <> hostPortToText hp
  where
    schemePart = case mscheme of
      Nothing -> ""
      Just s -> schemeText s <> "://"

instance FromJSON SchemeHostPort where
  parseJSON = withText "SchemeHostPort" $ \v -> either fail pure $ schemeHostPortFromText v

