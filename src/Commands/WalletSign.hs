{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.WalletSign
  ( quicksignCommand
  ) where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad.Except
import qualified Data.YAML.Aeson as YA
import           Kadena.SigningApi
import           Kadena.SigningTypes
import           Servant.API
import           Servant.Client
import           System.Exit
import           System.IO
import           Text.Printf
------------------------------------------------------------------------------
import           Types.Encoding
import           Types.Env
import           Utils
------------------------------------------------------------------------------

quicksignCommand :: Env -> WalletSignArgs -> IO ()
quicksignCommand env args = do
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
            printf "Done signing"
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

--sign :: SigningRequest -> ClientM SigningResponse
quicksign :: QuickSignRequest -> ClientM QuickSignResponse
oldSign :<|> quicksign = client signingAPI

chainweaverUrl :: BaseUrl
chainweaverUrl = BaseUrl Http "localhost" 9467 ""

signYamlFiles :: Env -> WalletSignArgs -> IO ()
signYamlFiles env args = do
  let files = _walletSignArgs_files args
      method = _walletSignArgs_method args
  ecsds <- mapM (signYamlFile method) files
  case partitionEithers ecsds of
    ([],csds) -> do
      case method of
        Quicksign -> do
          let qsReq = QuickSignRequest csds
              clientEnv = mkClientEnv (_env_httpManager env) chainweaverUrl
          eresp <- runClientM (quicksign qsReq) clientEnv
          case eresp of
            Left e -> error $ show e
            Right resp -> print $ unQuickSignResponse resp
        OldSign -> do
          eresps <- forM csds $ \csd -> do
            let qsReq = SigningRequest csds
                clientEnv = mkClientEnv (_env_httpManager env) chainweaverUrl
            runClientM (oldSign qsReq) clientEnv
          case partitionEithers eresps of
            ([], resps) -> do
              forM_ (zip files resps) $ \(f,r) -> do
                writeFile f r
            (es, _) -> error $ show e
    (es,_) -> do
      error $ printf "Got %d errors while parsing files\n%s" (length es) (unlines es)

csdToSigningRequest :: CommandSigData -> Either String SigningRequest
csdToSigningRequest csd = do
    code <-note "Cannot sign CONT transactions with the old signing API"
    d <-undefined
    caps <-undefined
    n <-undefined
    cid <-undefined
    gasLimit <-undefined
    ttl <-undefined
    sender <-undefined
    extraSigners <-undefined
    pure $ SigningRequest code d caps n cid gasLimit ttl sender extraSigners
  where
    (c,p) = commandSigDataToParsedCommand csd

--data Payload m c = Payload
--  { _pPayload :: !(PactRPC c)
--  , _pNonce :: !Text
--  , _pMeta :: !m
--  , _pSigners :: ![Signer]
--  , _pNetworkId :: !(Maybe NetworkId)
--  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
--data SigningRequest = SigningRequest
--  { _signingRequest_code :: Text
--  , _signingRequest_data :: Maybe Object
--  , _signingRequest_caps :: [DappCap]
--  , _signingRequest_nonce :: Maybe Text
--  , _signingRequest_chainId :: Maybe ChainId
--  , _signingRequest_gasLimit :: Maybe GasLimit
--  , _signingRequest_ttl :: Maybe TTLSeconds
--  , _signingRequest_sender :: Maybe AccountName
--  , _signingRequest_extraSigners :: Maybe [PublicKey]
--  } deriving (Show, Generic)

signYamlFile :: WalletSignMethod -> FilePath -> IO (Either String CommandSigData)
signYamlFile method msgFile = do
  mh <- openFile msgFile ReadWriteMode
  rawbs <- readAsEncoding Yaml mh
  hClose mh
  case YA.decode1Strict rawbs of
    Left _ -> die $ printf "Error: %s file contents does not match its extension." msgFile
    Right csd -> pure $ Right csd

signOther :: FilePath -> Encoding -> IO ()
signOther _ _ = do
  putStrLn "Not implemented yet"
