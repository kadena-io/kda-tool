{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Aeson as A
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Either
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Scientific
import           Data.Time
import qualified Data.YAML.Aeson as Y
import           GHC.Generics
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

instance FromJSON a => FromJSON (MaybeBatch a) where
  parseJSON v =
    case v of
      Object o -> do
        mcmds <- o .:? "cmds"
        case mcmds of
          Nothing -> parseJSON v
          Just cs -> pure $ MaybeBatch cs
      _ -> MaybeBatch . (:[]) <$> parseJSON v

parseAsJsonOrYaml :: FromJSON a => [LB.ByteString] -> Either [String] [a]
parseAsJsonOrYaml bss =
  case partitionEithers $ A.eitherDecode <$> bss of
    ([],vs) -> Right $ concat $ map unMaybeBatch vs
    (esJ,_) -> case partitionEithers $ Y.decode1Strict . LB.toStrict <$> bss of
      ([],vs) -> Right $ concat $ map unMaybeBatch vs
      (esY,_) -> Left (map show esJ ++ map show esY)

