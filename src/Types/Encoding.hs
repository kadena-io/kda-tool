{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Encoding where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Lens
import qualified Crypto.Hash as Crypto
import           Data.Aeson (Value(..))
import           Data.Aeson.Lens
import           Data.Bifunctor
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64Url
import           Data.ByteString.Base16
import qualified Data.ByteString.Base64 as B64
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.YAML.Aeson as YA
import           Data.Word
import           Kadena.SigningTypes
import           System.IO
------------------------------------------------------------------------------

data Encoding = Raw | B16 | B64 | B64Url | Yaml
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

encodingToText :: Encoding -> Text
encodingToText = \case
  Raw -> "raw"
  B16 -> "b16"
  B64 -> "b64"
  B64Url -> "b64url"
  Yaml -> "yaml"

textToEncoding :: Text -> Maybe Encoding
textToEncoding = \case
  "raw" -> Just Raw
  "b16" -> Just B16
  "b64" -> Just B64
  "b64url" -> Just B64Url
  "yaml" -> Just Yaml
  _ -> Nothing

genericDecode :: Encoding -> ByteString -> Either Text ByteString
genericDecode Raw = Right
genericDecode B16 = decodeBase16Untyped
genericDecode B64 = B64.decodeBase64Untyped
genericDecode B64Url = B64Url.decodeBase64Untyped
genericDecode Yaml = decodeYamlBS -- We don't actually use the result of this case

decodeYamlBS :: ByteString -> Either Text ByteString
decodeYamlBS bs = do
  v :: Value <- first (T.pack . snd) $ YA.decode1Strict bs
  let mhash = hush . B64Url.decodeBase64Untyped . encodeUtf8 =<< (v ^? key "hash" . _String)
      mcmd = encodeUtf8 <$> (v ^? key "cmd" . _String)
  case (mhash, mcmd) of
    (Nothing, Nothing) -> Left "YAML must contain a key 'hash' and/or 'cmd'"
    (Just hash, Nothing) -> Right hash
    (Nothing, Just cmd) -> Right $ calcCmdHash cmd
    (Just hash, Just cmd) ->
      if calcCmdHash cmd == hash
        then Right hash
        else Left $ T.unlines
               [ "DANGER!!! The hash does not match the command!  Someone may be trying to get you to sign something malicious!"
               , "If you are sure you want to proceed you should delete either the hash or the cmd (almost certainly the hash) from your YAML."
               , "PROCEED WITH GREAT CAUTION!!!"
               ]

calcCmdHash :: ByteString -> ByteString
calcCmdHash = BA.convert . Crypto.hashWith Crypto.Blake2b_256

calcCsdHash :: CommandSigData -> ByteString
calcCsdHash = calcCmdHash . encodeUtf8 . _csd_cmd

readAsEncoding :: Encoding -> Handle -> IO ByteString
readAsEncoding enc h = do
  bs <- B.hGetContents h
  let trimmer :: Word8 -> Bool
      trimmer w = case enc of
                    Raw -> False
                    _ -> isSpace $ chr (fromIntegral w)
  pure $ B.dropWhile trimmer $ B.dropWhileEnd trimmer bs

filenameToEncoding :: FilePath -> Maybe Encoding
filenameToEncoding file = textToEncoding extension
  where
    extension = T.takeWhileEnd (/= '.') $ T.pack file
