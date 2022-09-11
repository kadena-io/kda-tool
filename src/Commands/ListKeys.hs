{-# LANGUAGE OverloadedStrings #-}

module Commands.ListKeys where

------------------------------------------------------------------------------
import qualified Data.Text.IO as T
------------------------------------------------------------------------------
import           Keys
import           Utils
------------------------------------------------------------------------------

listKeysCommand :: FilePath -> KeyIndex -> IO ()
listKeysCommand keyfile ind = do
      Just phrase <- readPhraseFromFile keyfile
      let root = mnemonicToRoot phrase
          getAndShow n = tshow (unKeyIndex n) <> ": " <> pubKeyToText (snd $ generateCryptoPairFromRoot root "" n)
      mapM_ (T.putStrLn . getAndShow) [0..ind]
