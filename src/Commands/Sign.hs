{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Sign
  ( signCommand
  ) where

------------------------------------------------------------------------------
import qualified Cardano.Crypto.Wallet as Crypto
import           Control.Error
import qualified Crypto.Hash as Crypto
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import qualified Data.ByteArray as BA
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text.Encoding
import qualified Data.YAML.Aeson as YA
import           Kadena.SigningTypes
import           Pact.Types.Command
import           Pact.Types.SigData
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Printf
------------------------------------------------------------------------------
import           Keys
import           Types.Encoding
import           Types.Env
import           Utils
------------------------------------------------------------------------------

signCommand :: SignArgs -> IO ()
signCommand args = do
  let keyfile = _signArgs_keyFile args
  let index = _signArgs_keyInd args
  let files = _signArgs_files args

  -- NOTE: The most convenient signing UX is to take a list of files on the
  -- command line and sign them all with the supplied key, adding the
  -- signature to each file. However, this UX only works for YAML files that
  -- have a field for signatures. Therefore, if we want to sign any other
  -- encoding, we'll only be able to do that for a single message at a time
  -- because none of those encodings have provisions for adding a signature.
  if all hasYamlExtension files
    then do
      res <- runExceptT $ do
        kh <- lift $ fileOrStdin keyfile
        kkey <- fmapLT (\e -> "Error reading key material from " <> keyfile <> ": " <> e) $ ExceptT $ readKadenaKey kh
        case files of
          [] -> throwError "No files to sign"
          fs -> lift $ do
            mfileCounts <- mapM (signYamlFile kkey index) fs
            let fileCounts = catMaybes mfileCounts
            printf "Wrote %d signatures to the following files: %s\n" (sum $ map snd fileCounts) (intercalate ", " $ map fst fileCounts)
      case res of
        Left e -> putStrLn e >> exitFailure
        Right _ -> pure ()
    else case files of
      [] -> putStrLn "No files to sign"
      [f] ->
        case filenameToEncoding f of
          Nothing -> printf "Error: %s has an unrecognized extension.  Must be one of raw, b16, b64, b64url, or yaml." f >> exitFailure
          Just enc -> signOther f enc
      _ -> putStrLn "Cannot sign more than one non-yaml file at a time" >> exitFailure

signYamlFile :: KadenaKey -> Maybe KeyIndex -> FilePath -> IO (Maybe (FilePath, Int))
signYamlFile kkey mindex msgFile = do
  mh <- openFile msgFile ReadWriteMode
  rawbs <- readAsEncoding Yaml mh
  hClose mh
  case YA.decode1Strict rawbs of
    Left _ -> die $ printf "Error: %s file contents does not match its extension." msgFile
    Right csd -> do
      let sigs = _csd_sigs csd
          cmd = _csd_cmd csd
          signingKeys = S.fromList $ map fst $ unSignatureList sigs
      case kkey of
        HDRoot xprv -> do
          tryHdIndex msgFile csd xprv mindex
        PlainKeyPair sec pub -> do
          let pubHex = PublicKeyHex $ toB16 $ BA.convert pub
          if S.member pubHex signingKeys
            then do
              let sig = UserSig $ toB16 $ BA.convert $ sign sec (calcHash $ encodeUtf8 cmd)
              hClose mh
              let newSigs = addSig pubHex sig sigs
              let csd2 = CommandSigData newSigs cmd
              fp <- saveCommandSigData (dropExtension msgFile) csd2
              pure $ Just (fp, countSigs csd2 - countSigs csd)
            else pure Nothing

tryHdIndex
  :: FilePath
  -> CommandSigData
  -> Crypto.XPrv
  -> Maybe KeyIndex
  -> IO (Maybe (FilePath, Int))
tryHdIndex msgFile csd xprv mind = do
  let startingSigs = _csd_sigs csd
      cmd = _csd_cmd csd
      cmdBS = encodeUtf8 cmd
      signingKeys = S.fromList $ map fst $ unSignatureList startingSigs
      signPairs = getSigningInds signingKeys xprv (maybe [0..100] (:[]) mind)
      f (esec, pub) = addSig pub (UserSig $ sigToText $ signHD esec (calcHash cmdBS))
      newSigs = foldr f startingSigs signPairs
  let csd2 = CommandSigData newSigs cmd
      num1 = countSigs csd
      num2 = countSigs csd2
  if num2 > num1
    then do
      fp <- saveCommandSigData (dropExtension msgFile) csd2
      pure $ Just (fp, countSigs csd2 - countSigs csd)
    else pure Nothing

calcHash :: ByteString -> ByteString
calcHash = BA.convert . Crypto.hashWith Crypto.Blake2b_256

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
--signOther msgFile enc = do
signOther _ _ = do
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
