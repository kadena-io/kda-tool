{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.GenTx where

------------------------------------------------------------------------------
import           Control.Applicative
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
import qualified Data.Vector as V
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

genTxCommand :: Env -> GenTxArgs -> IO ()
genTxCommand e args = do
  etplContents <- case _genTxArgs_template args of
    TemplateFile f -> Right <$> T.readFile f
    TemplateGitHub ght -> githubFile e ght
  case etplContents of
    Left msg -> putStrLn msg
    Right tplContents -> genFromContents (_genTxArgs_operation args) tplContents

githubFile :: Env -> GitHubTemplate -> IO (Either String Text)
githubFile e ght = do
  let repos = fromMaybe (["kadena-io/txlib"]) $ (fmap (:[]) (_ght_templateRepo ght) <|> _configData_txRepos (_env_configData e))
      tpl = _ght_templateName ght
      mkUrl repo = printf "https://raw.githubusercontent.com/%s/master/%s.ktpl" repo tpl
      go repo = do
        httpsMgr <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
        req <- parseRequest (mkUrl repo)
        resp <- httpLbs req httpsMgr
        let tplContents = toS $ responseBody resp
        if statusIsSuccessful $ responseStatus resp
          then return $ Right tplContents
          else return $ Left $ printf "Could not find template '%s' in repos %s\n"
                        (_ght_templateName ght) (show repos)
  untilRight (go <$> repos)

untilRight :: Monad m => [m (Either String a)] -> m (Either String a)
untilRight [] = pure $ Left "untilRight: no repos found"
untilRight (m:ms) = do
  ma <- m
  let f e = case ms of
        [] -> pure $ Left e
        _ -> untilRight ms
  either f (pure . Right) ma

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
        txts <- hoistEither $ first prettyFailure $ fillValueVars tpl augmentedVars
        txis :: [TxInputs] <- sequence $ map (hoistEither . parseTxInputs) txts
        apiReqs :: [Pact.ApiReq] <- mapM (lift . txInputsToApiReq) txis
        cmds :: [Command Text] <- mapM (fmap snd . lift . Pact.mkApiReqCmd True "") apiReqs
        let sds :: [SigData Text] = rights $ map commandToSigData cmds
        let outs :: [Text] = map (decodeUtf8 . LB.toStrict . YA.encode1) sds
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
    if S.member "chain" (M.keysSet onlyArrays)
      then "tx-{{{chain}}}.yaml"
      else case M.toList onlyArrays of
             ((k,Array v):_) ->
               if V.length v > 1
                 then T.pack $ printf "tx-{{{%s}}}.yaml" k
                 else "tx.yaml"

             -- We really only want to match [] here but the other cases should
             -- never happen because we're filtering on isArray, and even if
             -- they do happen they would generate errors elsewhere and this
             -- code would not matter.
             _ -> "tx.yaml"
  where
    onlyArrays = M.filter isArray m

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
