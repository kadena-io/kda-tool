{-# LANGUAGE DeriveGeneric #-}

module Commands.Send
  ( SendArgs(..)
  , sendCommand
  , sendP
  ) where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Options.Applicative
------------------------------------------------------------------------------
import           Types.HostPort
------------------------------------------------------------------------------

data SendArgs = SendArgs
  { _sendArgs_files :: [FilePath]
  , _sendArgs_node :: HostPort
  } deriving (Eq,Ord,Show,Read,Generic)

sendP :: Parser SendArgs
sendP = SendArgs
  <$> many (strArgument (help "File(s) containing command(s) to send"))
  <*> option (eitherReader (hostPortFromText . T.pack))
             (long "node" <> short 'n' <>
              help "Node hostname and optional port separated by a ':'")

sendCommand :: SendArgs -> IO ()
sendCommand args = do
  print args

