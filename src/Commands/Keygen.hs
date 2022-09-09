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
--import           Pact.Types.Crypto
------------------------------------------------------------------------------
import           Keys
import           Types.Env
import           Types.KeyType
import           Utils
------------------------------------------------------------------------------

keygenCommand :: Env -> KeyType -> IO ()
keygenCommand e kt = do
    case kt of
      Plain -> do
        putStrLn "Not implemented yet"

        -- Getting a linker error
        -- multiple definition of 'batch_point_buffer'

        --kp <- genKeyPair defaultScheme
        --putStrLn $ "public: " ++ T.unpack (encodeBase16 $ getPublic kp)
        --putStrLn $ "secret: " ++ T.unpack (encodeBase16 $ getPrivate kp)
      HD -> do
        let toPhrase = T.unwords . M.elems . mkPhraseMapFromMnemonic
        let prettyErr e = "ERROR generating menmonic: " <> tshow e
        res <- either prettyErr toPhrase <$> genMnemonic12
        T.putStrLn res
