{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Sign
  ( signCommand
  ) where

------------------------------------------------------------------------------
import qualified Cardano.Crypto.Wallet as Crypto
import           Control.Error
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson hiding (Encoding)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.YAML as Y
import qualified Data.YAML.Aeson as YA
import qualified Data.YAML.Event as Y
import qualified Data.YAML.Schema as Y
import qualified Data.YAML.Token as Y
import           Kadena.SigningTypes
import           Pact.Types.Command
import           Pact.Types.SigData
import           System.Exit
import           System.IO
import           Text.Printf
------------------------------------------------------------------------------
import           Keys
import           Types.Encoding
import           Types.Env
------------------------------------------------------------------------------

signCommand :: SignArgs -> IO ()
signCommand args = do
    let keyfile = _signArgs_keyFile args
    let index = _signArgs_keyInd args
    let enc = _signArgs_encoding args

    -- NOTE: The most convenient signing UX is to take a list of files on the
    -- command line and sign them all with the supplied key, adding the
    -- signature to each file. However, this UX only works for YAML files that
    -- have a field for signatures. Therefore, if we want to sign any other
    -- encoding, we'll only be able to do that for a single message at a time
    -- because none of those encodings have provisions for adding a signature.

    res <- runExceptT $ do
      kh <- lift $ fileOrStdin keyfile
      kkey <- fmapLT (\e -> "Error reading key material from " <> keyfile <> ": " <> e) $ ExceptT $ readKadenaKey kh
      case _signArgs_files args of
        [] -> throwError "No files to sign"
        fs -> void $ lift $ mapM_ (signOne kkey index) fs
    case res of
      Left e -> putStrLn e >> exitFailure
      Right _ -> hPutStrLn stderr "Done."

signOne :: KadenaKey -> Maybe KeyIndex -> FilePath -> IO ()
signOne kkey mindex msgFile = do
    case filenameToEncoding msgFile of
      Nothing -> printf "Error: %s has an unrecognized extension.  Must be one of raw, b16, b64, b64url, or yaml." msgFile >> exitFailure
      Just Yaml -> signYamlFile msgFile kkey mindex
      Just enc -> signOther msgFile enc

signYamlFile :: FilePath -> KadenaKey -> Maybe KeyIndex -> IO ()
signYamlFile msgFile kkey mindex = do
  mh <- openFile msgFile ReadWriteMode
  rawbs <- readAsEncoding Yaml mh
  hClose mh
  case YA.decode1Strict rawbs of
    Left e -> die $ printf "Error: %s file contents does not match its extension." msgFile
    Right csd -> do
      let sigs = _csd_sigs csd
          cmd = _csd_cmd csd
          signingKeys = S.fromList $ map fst $ unSignatureList sigs
      case kkey of
        HDRoot xprv -> tryHdIndex msgFile csd xprv mindex
        PlainKeyPair sec pub -> do
          let pubHex = PublicKeyHex $ toB16 $ BA.convert pub
          when (S.member pubHex signingKeys) $ do
            let sig = UserSig $ toB16 $ BA.convert $ sign sec (encodeUtf8 cmd)
            hClose mh
            mh2 <- openFile msgFile ReadWriteMode
            let newSigs = addSig pubHex sig sigs
            let csd = CommandSigData newSigs cmd
            let outBS = niceQuoteEncode csd
            LB.hPut mh2 outBS
            hClose mh2

niceQuoteEncode :: ToJSON a => a -> LB.ByteString
niceQuoteEncode a = YA.encodeValue' senc Y.UTF8 [toJSON a]
  where
    encScalar s@(Y.SStr t) = case T.find (== '"') t of
      Just _ -> Right (Y.untagged, Y.SingleQuoted, t)
      Nothing -> Y.schemaEncoderScalar Y.coreSchemaEncoder s
    encScalar s = Y.schemaEncoderScalar Y.coreSchemaEncoder s
    senc = Y.setScalarStyle encScalar Y.coreSchemaEncoder

tryHdIndex
  :: FilePath
  -> CommandSigData
  -> Crypto.XPrv
  -> Maybe KeyIndex
  -> IO ()
tryHdIndex msgFile csd xprv mind = do
  let startingSigs = _csd_sigs csd
      cmd = _csd_cmd csd
      cmdBS = encodeUtf8 cmd
      signingKeys = S.fromList $ map fst $ unSignatureList startingSigs
      signPairs = getSigningInds signingKeys xprv (maybe [0..100] (:[]) mind)
      f (esec, pub) = addSig pub (UserSig $ sigToText $ signHD esec cmdBS)
      newSigs = foldr f startingSigs signPairs
  mh2 <- openFile msgFile ReadWriteMode
  let csd = CommandSigData newSigs cmd
  let outBS = YA.encode1Strict csd
  B.hPut mh2 outBS
  hClose mh2

getSigningInds
  :: Set PublicKeyHex
  -> Crypto.XPrv
  -> [KeyIndex]
  -> [(EncryptedPrivateKey, PublicKeyHex)]
getSigningInds signingKeys xprv inds = filter inSigningKeys pairs
  where
    pairs = map (mkPair . generateCryptoPairFromRoot xprv "") inds
    mkPair (esec, pub) = (esec, PublicKeyHex $ pubKeyToText pub)
    inSigningKeys pair = S.member (snd pair) signingKeys

addSig :: PublicKeyHex -> UserSig -> SignatureList -> SignatureList
addSig pub sig (SignatureList sigs) = SignatureList $ go sigs
  where
    go [] = []
    go (pair@(k,_):ps) = if k == pub then (k, Just sig) : go ps else pair : go ps

signOther :: FilePath -> Encoding -> IO ()
signOther msgFile enc = do
  putStrLn "Not implemented yet"
--  rawbs <- readAsEncoding enc mh
--  let ebs = genericDecode enc rawbs
--  case ebs of
--    Left e -> die $ printf "Error: %s file contents does not match its extension." msgFile
--    Right msg -> do
--      let pub = getMaterialPublic material
--          sig = signWithMaterial material msg
--      case enc of
--        Yaml -> do
--          let res = do
--                v :: Value <- first  (T.pack . snd) $ YA.decode1Strict rawbs
--                pure (v & key "sigs" . key pub .~ String (decodeUtf8 sig))
--              encScalar s@(Y.SStr t) = case T.find (== '"') t of
--                Just _ -> Right (Y.untagged, Y.SingleQuoted, t)
--                Nothing -> Y.schemaEncoderScalar Y.coreSchemaEncoder s
--              encScalar s = Y.schemaEncoderScalar Y.coreSchemaEncoder s
--              senc = Y.setScalarStyle encScalar Y.coreSchemaEncoder
--          case res of
--            Right v -> LB.putStrLn $ YA.encodeValue' senc Y.UTF8 [v]
--            Left e -> die $ T.unpack e
--        _ -> do
--          T.putStrLn $ pub <> ": " <> decodeUtf8 sig
