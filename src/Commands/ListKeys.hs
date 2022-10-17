{-# LANGUAGE OverloadedStrings #-}

module Commands.ListKeys where

------------------------------------------------------------------------------
import qualified Data.ByteArray as BA
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Printf
------------------------------------------------------------------------------
import           Keys
import           Types.Env
import           Utils
------------------------------------------------------------------------------

listKeysCommand :: Either FilePath ChainweaverFile -> KeyIndex -> IO ()
listKeysCommand efc ind = do
  (keyfile, h) <- getKeyFile efc
  ekey <- readKadenaKey h
  case ekey of
    Left e -> printf "Error reading key from %s: %s\n" keyfile e
    Right (HDRoot xprv mpass) -> do
      let pass = fromMaybe "" mpass
      let getAndShow n = tshow (unKeyIndex n) <> ": " <> pubKeyToText (snd $ generateCryptoPairFromRoot xprv pass n)
      mapM_ (T.putStrLn . getAndShow) [0..ind]
    Right (PlainKeyPair _ pub) -> do
      let pubHex = toB16 $ BA.convert pub
      putStrLn $ "public: " <> T.unpack pubHex
