{-# LANGUAGE OverloadedStrings #-}

module Types.KeyType where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           GHC.Generics
------------------------------------------------------------------------------

data KeyType = Plain | HD
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

keyTypeToText :: KeyType -> Text
keyTypeToText Plain = "plain"
keyTypeToText HD = "hd"

keyTypeFromText :: Text -> Either String KeyType
keyTypeFromText "plain" = Right Plain
keyTypeFromText "hd" = Right HD
keyTypeFromText t = Left ("Invalid KeyType: " <> T.unpack t)
