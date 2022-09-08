module Types.KeyType where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Read
import           GHC.Generics
------------------------------------------------------------------------------

data KeyType = Plain | HD
  deriving (Eq,Ord,Show,Read)

keyTypeToText :: KeyType -> Text
keyTypeToText Plain = "plain"
keyTypeToText HD = "hd"

keyTypeFromText :: Text -> Either String KeyType
keyTypeFromText "plain" = Plain
keyTypeFromText "hd" = HD
keyTypeFromText t = Left "(Invalid KeyType: " <> T.unpack t)
