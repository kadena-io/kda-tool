{-# LANGUAGE FlexibleContexts #-}
module Utils where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString.Char8 as A (endOfInput, parseOnly, scientific)
import qualified Data.ByteString as B
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Scientific
import           Data.Time
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
