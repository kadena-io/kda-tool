{-# LANGUAGE DeriveGeneric #-}

module Commands.Keygen
  ( localCommand
  ) where

------------------------------------------------------------------------------
import           Chainweb.Api.Transaction
import           Control.Error
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import           Data.String.Conv
import qualified Data.Text.IO as T
import           Katip
import           System.Exit
import           Text.Printf
------------------------------------------------------------------------------
import           Types.Env
import           Types.Node
import           Utils
------------------------------------------------------------------------------

keygenCommand :: Env -> KeyType -> IO ()
keygenCommand e kt = do
    let toPhrase = T.unwords . Map.elems . mkPhraseMapFromMnemonic
    let prettyErr e = "ERROR generating menmonic: " <> tshow e
    res <- either prettyErr toPhrase <$> genMnemonic12
    T.putStrLn res
