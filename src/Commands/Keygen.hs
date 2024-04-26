{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Keygen
  ( keygenCommand
  ) where

------------------------------------------------------------------------------
import           Data.ByteString.Base16
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Pact.Types.Crypto
------------------------------------------------------------------------------
import           Keys
import           Types.KeyType
import           Utils
import Data.Base16.Types (extractBase16)
------------------------------------------------------------------------------

keygenCommand :: KeyType -> IO ()
keygenCommand kt = do
  case kt of
    Plain -> do
      kp <- genKeyPair
      putStrLn $ "public: " ++ T.unpack (extractBase16 $ encodeBase16 $ getPublic kp)
      putStrLn $ "secret: " ++ T.unpack (extractBase16 $ encodeBase16 $ getPrivate kp)
    HD -> do
      let toPhrase = T.unwords . M.elems . mkPhraseMapFromMnemonic
      let prettyErr err = "ERROR generating menmonic: " <> tshow err
      res <- either prettyErr toPhrase <$> genMnemonic12
      T.putStrLn res
