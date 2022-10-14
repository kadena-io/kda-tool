{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.WalletSign
  ( walletSignCommand
  ) where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson hiding (Encoding)
import           Data.Aeson.Lens
import           Data.List
import qualified Data.Set as S
import           Data.String.Conv
import qualified Data.Text.IO as T
import qualified Data.YAML.Aeson as YA
import           Kadena.SigningApi
import           Kadena.SigningTypes
import           Pact.Types.Capability
import           Pact.Types.ChainMeta
import           Pact.Types.Command
import           Pact.Types.KeySet
import           Pact.Types.Names
import           Pact.Types.RPC
import           Servant.API
import           Servant.Client
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Printf
------------------------------------------------------------------------------
import           Types.Encoding
import           Types.Env
import           Utils
------------------------------------------------------------------------------

walletSignCommand :: Env -> WalletSignArgs -> IO ()
walletSignCommand env args = do
  let files = _walletSignArgs_files args
  -- NOTE: The most convenient signing UX is to take a list of files on the
  -- command line and sign them all with the supplied key, adding the
  -- signature to each file. However, this UX only works for YAML files that
  -- have a field for signatures. Therefore, if we want to sign any other
  -- encoding, we'll only be able to do that for a single message at a time
  -- because none of those encodings have provisions for adding a signature.
  if all hasYamlExtension files
    then do
      res <- runExceptT $ do
        case files of
          [] -> throwError "No files to sign"
          _ -> lift $ do
            signYamlFiles env args
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

oldSign :: SigningRequest -> ClientM SigningResponse
quicksign :: QuickSignRequest -> ClientM QuickSignResponse
oldSign :<|> quicksign = client signingAPI

chainweaverUrl :: BaseUrl
chainweaverUrl = BaseUrl Http "localhost" 9467 ""

signYamlFiles :: Env -> WalletSignArgs -> IO ()
signYamlFiles env args = do
  let files = _walletSignArgs_files args
      meth = _walletSignArgs_method args
  ecsds <- mapM signYamlFile files
  case partitionEithers ecsds of
    ([],csds) -> do
      case meth of
        Quicksign -> do
          let qsReq = QuickSignRequest csds
              clientEnv = mkClientEnv (_env_httpManager env) chainweaverUrl
          eresp <- runClientM (quicksign qsReq) clientEnv
          case eresp of
            Left e -> error $ show e
            Right resp -> do
              let newCsds = unQuickSignResponse resp
                  saveAndCountSigs (f,c1,c2) = do
                    f2 <- saveCommandSigData (dropExtension f) c2
                    pure (f2, countSigs c2 - countSigs c1)
              fileCounts <- mapM saveAndCountSigs $ zip3 files csds newCsds
              printf "Wrote %d signatures to the following files: %s\n" (sum $ map snd fileCounts) (intercalate ", " $ map fst fileCounts)
              --case resp of
              --  QSR_Response [CSDResponse] -> print $ unQuickSignResponse resp
              --  QSR_Error e -> error $ "Got error from QuickSign:\n" <> show e
        OldSign -> do
          eresps <- runExceptT $ forM csds $ \csd -> do
            sreq <- hoistEither $ csdToSigningRequest csd
            let clientEnv = mkClientEnv (_env_httpManager env) chainweaverUrl
            fmapLT show $ ExceptT $ runClientM (oldSign sreq) clientEnv
          case eresps of
            Left e -> error e
            Right resps -> do
              forM_ (zip files resps) $ \(f,r) -> do
                T.writeFile f $ toS $ encode $ _signingResponse_body r
    (es,_) -> do
      error $ printf "Got %d errors while parsing files\n%s" (length es) (unlines es)

csdToSigningRequest :: CommandSigData -> Either String SigningRequest
csdToSigningRequest csd = do
    (_,p) <- commandSigDataToParsedCommand csd
    case _pPayload p of
      Continuation _ -> Left "Cannot sign CONT transactions with the old signing API"
      Exec m -> do
        let code = _pcCode $ _pmCode m
            d = _pmData m ^? _Object
        let caps = map mkDappCap $ S.toList $ S.fromList $ concatMap _siCapList $ _pSigners p
        let n = Just $ _pNonce p
            meta = _pMeta p
            cid = Just $ _pmChainId meta
            gasLimit = Just $ _pmGasLimit meta
            ttl = Just $ _pmTTL meta
            sender = Just $ AccountName $ _pmSender meta
            extraSigners = case map (PublicKey . toS . _siPubKey) $ filter (\s -> null $ _siCapList s) $ _pSigners p of
              [] -> Nothing
              ks -> Just ks
        pure $ SigningRequest code d caps n cid gasLimit ttl sender extraSigners

mkDappCap :: SigCapability -> DappCap
mkDappCap sc = DappCap (_qnName $ _scName sc) "desc" sc

signYamlFile :: FilePath -> IO (Either String CommandSigData)
signYamlFile msgFile = do
  mh <- openFile msgFile ReadWriteMode
  rawbs <- readAsEncoding Yaml mh
  hClose mh
  case YA.decode1Strict rawbs of
    Left _ -> die $ printf "Error: %s file contents does not match its extension." msgFile
    Right csd -> pure $ Right csd

signOther :: FilePath -> Encoding -> IO ()
signOther _ _ = do
  putStrLn "Not implemented yet"
