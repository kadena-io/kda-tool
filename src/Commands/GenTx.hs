{-# LANGUAGE OverloadedStrings #-}

module Commands.GenTx where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.YAML.Aeson as YA
import           Network.Connection
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status
--import           Pact.ApiReq
import qualified Pact.ApiReq as Pact
import           Pact.Types.Command
import           Pact.Types.SigData
import           System.IO
import           Text.Printf
------------------------------------------------------------------------------
import           TxTemplate
import           Types.TxInputs
import           Types.Env
import           Utils
------------------------------------------------------------------------------

genTxCommand :: GenTxArgs -> IO ()
genTxCommand args = do
  tplContents <- T.readFile $ _genTxArgs_templateFile args
  genFromContents (_genTxArgs_operation args) tplContents

txCommand :: Env -> TxArgs -> IO ()
txCommand e args = do
  let repo = fromMaybe "kadena-io/txlib" $ _configData_txRepo $ _env_configData e
      tpl = _txArgs_templateName args
      url = printf "https://raw.githubusercontent.com/%s/master/%s.ktpl" repo tpl
  httpsMgr <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
  req <- parseRequest url
  resp <- httpLbs req httpsMgr
  let tplContents = toS $ responseBody resp
  if statusIsSuccessful $ responseStatus resp
    then genFromContents (Right $ GenData Nothing Nothing) tplContents
    else printf "Could not find template '%s' in repo %s\n" tpl repo

genFromContents :: Either Holes GenData -> Text -> IO ()
genFromContents op tplContents = do
  res <- runExceptT $ do
    (tpl,holes :: S.Set Text) <- hoistEither $ parseAndGetVars tplContents
    case op of
      Left _ -> do
        lift $ mapM_ (\h -> T.putStrLn $ h <> ": null") holes
        pure []
      Right gd -> do
        dataContents <- lift $ maybe (pure "{}") T.readFile $ _genData_dataFile gd
        vars :: M.Map Text Value <- hoistEither $ readVars dataContents
        let remainingHoles = map fst $ filter (isEmptyHole . snd) $ map (\k -> (k, M.lookup k vars)) $ S.toList holes
        rest <- if null remainingHoles
          then pure mempty
          else lift $ do
            hSetBuffering stdout NoBuffering
            vs <- mapM askForValue remainingHoles
            pure $ M.fromList vs

        -- A little magical convenience for the chain field
        let augmentedVars = M.adjust stringifyChain "chain" $
                              M.union (M.filter (/= Null) vars) rest
        lift $ putStrLn $ "augmentedVars: "  <> show augmentedVars
        txts <- hoistEither $ first prettyFailure $ fillValueVars tpl augmentedVars
        lift $ putStrLn $ "txts: "  <> show txts
        txis :: [TxInputs] <- sequence $ map (hoistEither . parseTxInputs) txts
        lift $ putStrLn $ "txis: "  <> show txis
        apiReqs :: [Pact.ApiReq] <- mapM (lift . txInputsToApiReq) txis
        lift $ putStrLn $ "apiReqs: "  <> show apiReqs
        cmds :: [Command Text] <- mapM (fmap snd . lift . Pact.mkApiReqCmd True "") apiReqs
        lift $ putStrLn $ "cmds: "  <> show cmds
        let sds :: [SigData Text] = rights $ map commandToSigData cmds
        lift $ putStrLn $ "sds: "  <> show sds
        let outs :: [Text] = map (decodeUtf8 . LB.toStrict . YA.encode1) sds
        lift $ putStrLn $ "outs: "  <> show outs
        let outPat = maybe (defaultOutPat augmentedVars) T.pack $ _genData_outFilePat gd
        (fpTmpl, fpVars) <- hoistEither $ parseAndGetVars outPat
        fps <- hoistEither $ first prettyFailure $ fillFilenameVars fpTmpl (M.restrictKeys augmentedVars fpVars)
        let ps = zip fps outs
        lift $ mapM_ (\(fp,cmd) -> T.writeFile (T.unpack fp) cmd) ps
        pure ps
  case res of
    Left e -> error e
    Right [] -> pure ()
    Right ps -> putStrLn $ "Wrote commands to: " <> show (map fst ps)

readVars :: Text -> Either String (M.Map Text Value)
readVars dataContents = first show $ YA.decode1 (LB.fromStrict $ encodeUtf8 dataContents)

stringifyChain :: Value -> Value
stringifyChain (Number x) = String $ tshow (round x :: Int)
stringifyChain v = v

parseTxInputs :: Text -> Either String TxInputs
parseTxInputs = first show . YA.decode1Strict . encodeUtf8

defaultOutPat :: M.Map Text Value -> Text
defaultOutPat m =
    if S.member "chain" arrayKeys
      then "tx-{{chain}}.yaml"
      else case S.toList arrayKeys of
             [] -> "tx.yaml"
             (t:_) -> T.pack $ printf "tx-{{%s}}.yaml" t
  where
    arrayKeys = M.keysSet $ M.filter isArray m

askForValue :: Text -> IO (Text, Value)
askForValue k = do
  T.putStr (k <> ": ")
  str <- T.getLine
  case YA.decode1 (LB.fromStrict $ encodeUtf8 str) of
    Left _ -> putStrLn "Not a valid YAML value" >> askForValue k
    Right v -> pure (k, v)


isArray :: Value -> Bool
isArray (Array _) = True
isArray _ = False

isEmptyHole :: Maybe Value -> Bool
isEmptyHole Nothing = True
isEmptyHole (Just v) = v == Null
