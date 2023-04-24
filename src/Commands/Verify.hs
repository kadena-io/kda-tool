{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Verify
  ( verifyCommand
  ) where

------------------------------------------------------------------------------
import           Control.Error
import qualified Data.Text as T
import           System.Exit
import           System.IO
------------------------------------------------------------------------------
import           Keys
import           Types.Encoding
import           Types.Env
------------------------------------------------------------------------------

verifyCommand :: VerifyArgs -> IO ()
verifyCommand args = do
  let pubKey = _verifyArgs_pubKey args
  let sig = _verifyArgs_sig args
  let msgFile = _verifyArgs_msgFile args
  let enc = fromMaybe B64Url $ _verifyArgs_encoding args

  mh <- openFile msgFile ReadMode
  ebs <- genericDecode enc <$> readAsEncoding enc mh
  case ebs of
    Left e -> die $ "Error reading message file: " <> T.unpack e
    Right bs -> do
      if verify pubKey sig bs
        then putStrLn "Signature valid."
        else die "Invalid signature!"
