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
import           Data.Text (Text)
import qualified Data.Text.IO as T
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
  let mIndex = _signArgs_keyInd args
  let files = _signArgs_files args
  let enc = fromMaybe Yaml $ _signArgs_encoding args

  -- NOTE: The most convenient signing UX is to take a list of files on the
  -- command line and sign them all with the supplied key, adding the
  -- signature to each file. However, this UX only works for YAML files that
  -- have a field for signatures. Therefore, if we want to sign any other
  -- encoding, we'll only be able to do that for a single message at a time
  -- because none of those encodings have provisions for adding a signature.
  res <- runExceptT $ do
    (keyfile, kh) <- lift $ getKeyFile $ _signArgs_keyFile args
    kkey <- fmapLT (\e -> "Error reading key material from " <> keyfile <> ": " <> e) $ ExceptT $ readKadenaKey kh
    if all hasYamlExtension files && enc == Yaml
      then do
          case files of
            [] -> throwError "No files to sign"
            fs -> lift $ do
              mfileCounts <- mapM (signYamlFile kkey mIndex enc) fs
              let fileCounts = catMaybes mfileCounts
              printf "Wrote %d signatures to the following files: %s\n" (sum $ map snd fileCounts) (intercalate ", " $ map fst fileCounts)
      else case files of
        [] -> lift $ putStrLn "No files to sign"
        [f] -> lift $
          case mIndex of
            Nothing -> die "Must supply an HD key index with -i"
            Just ind -> signOther f kkey ind enc
        _ -> lift $ putStrLn "Cannot sign more than one non-yaml file at a time" >> exitFailure
  case res of
    Left e -> putStrLn e >> exitFailure
    Right _ -> pure ()

signYamlFile
  :: KadenaKey
  -> Maybe KeyIndex
  -> Encoding
  -> FilePath
  -> IO (Maybe (FilePath, Int))
signYamlFile kkey mindex enc msgFile = do
  mh <- openFile msgFile ReadWriteMode
  rawbs <- readAsEncoding enc mh
  hClose mh
  case YA.decode1Strict rawbs of
    Left _ -> die $ printf "Error: %s file contents does not match its extension." msgFile
    Right csd -> do
      let sigs = _csd_sigs csd
          cmd = _csd_cmd csd
          signingKeys = S.fromList $ map _s_pubKey $ unSignatureList sigs
      case kkey of
        HDRoot xprv mpass -> tryHdIndex msgFile csd xprv mpass mindex
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
  -> Maybe Text
  -> Maybe KeyIndex
  -> IO (Maybe (FilePath, Int))
tryHdIndex msgFile csd xprv mpass mind = do
  let startingSigs = _csd_sigs csd
      cmd = _csd_cmd csd
      cmdBS = encodeUtf8 cmd
      signingKeys = S.fromList $ map _s_pubKey $ unSignatureList startingSigs
      signPairs = getSigningInds signingKeys xprv mpass (maybe [0..100] (:[]) mind)
      f (esec, pub) = addSig pub (UserSig $ sigToText $ signHD esec (fromMaybe "" mpass) (calcHash cmdBS))
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
  -> Maybe Text
  -> [KeyIndex]
  -> [(EncryptedPrivateKey, PublicKeyHex)]
getSigningInds signingKeys xprv mpass inds = filter inSigningKeys pairs
  where
    pairs = map (mkPair . generateCryptoPairFromRoot xprv (fromMaybe "" mpass)) inds
    mkPair (esec, pub) = (esec, PublicKeyHex $ pubKeyToText pub)
    inSigningKeys pair = S.member (snd pair) signingKeys

addSig :: PublicKeyHex -> UserSig -> SignatureList -> SignatureList
addSig pub sig (SignatureList sigs) = SignatureList $ go sigs
  where
    go [] = []
    go (c:cs) =
      let k = _s_pubKey c
       in if k == pub then (CSDSigner k (Just sig)) : go cs else c : go cs

signOther :: FilePath -> KadenaKey -> KeyIndex -> Encoding -> IO ()
signOther msgFile kkey kind enc = do
  mh <- fileOrStdin msgFile
  rawbs <- readAsEncoding enc mh
  res <- runExceptT $ do
    let mkParseErr e = printf "Error parsing %s as %s:\n%s"
          msgFile (encodingToText enc) (show e)
    msg <- fmapLT mkParseErr $
      hoistEither $ genericDecode enc rawbs
    let (pubKey, sig) = case kkey of
          HDRoot xprv mpass ->
            let (esec, pub) = generateCryptoPairFromRoot xprv (fromMaybe "" mpass) kind
            in (pub, sigToText $ signHD esec (fromMaybe "" mpass) msg)
          PlainKeyPair sec pub -> (PublicKey $ BA.convert pub, toB16 $ BA.convert $ sign sec msg)
    lift $ T.putStrLn $ pubKeyToText pubKey <> ": " <> sig
  case res of
    Left e -> die e
    Right _ -> pure ()
