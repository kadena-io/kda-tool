{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AppMain where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Katip
import           Network.HTTP.Client hiding (withConnection)
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           System.IO
import           System.Random.MWC
------------------------------------------------------------------------------
import           Commands.Cut
import           Commands.CombineSigs
import           Commands.GenTx
import           Commands.Keygen
import           Commands.ListKeys
import           Commands.Local
import           Commands.Poll
import           Commands.Send
import           Commands.Sign
import           Options.Applicative.Help.Pretty
import           Types.Env
------------------------------------------------------------------------------

appMain :: IO ()
appMain = do
    Args c sev <- execParser opts
    let verbosity = V2
    mgr <- newManager tlsManagerSettings

    s1 <- liftIO $ mkHandleScribe ColorIfTerminal stderr (permitItem sev) verbosity
    le <- liftIO $ registerScribe "stderr" s1 defaultScribeSettings
      =<< initLogEnv "myapp" "production"

    logLE le DebugS $ logStr $ "Logging with severity " <> show sev
    rand <- createSystemRandom
    let theEnv = Env mgr le rand
    case c of
      --Batch files -> batchCommand files
      Cut hp -> cutCommand hp
      CombineSigs files -> combineSigsCommand theEnv files
      GenTx args -> genTxCommand args
      Keygen keyType -> keygenCommand keyType
      ListKeys kf ind -> listKeysCommand kf ind
      Local args -> localCommand theEnv args
      Poll args -> pollCommand theEnv args
      Send args -> sendCommand theEnv args
      Sign args -> signCommand args

  where
    opts = info (envP <**> helper) $ mconcat
      [ fullDesc
      , header "kda - Command line tool for interacting with the Kadena blockchain"
      , footerDoc (Just theFooter)
      ]
    theFooter = string $ unlines
      [ "Run the following command to enable tab completion:"
      , ""
      , "source <(kda --bash-completion-script `which kda`)"
      ]

fireNothing :: a -> IO ()
fireNothing _ = pure ()

batchCommand :: [FilePath] -> IO ()
batchCommand _ = do
  putStrLn "Not implemented yet"
--  bss <- mapM LB.readFile files
--  case partitionEithers $ A.eitherDecode <$> bss of
--    ([],vs) -> batchJSONs vs
--    (esJ,_) -> case partitionEithers $ Y.decode1Strict . LB.toStrict <$> bss of
--      ([],vs) -> batchYAMLs vs
--      (esY,_) -> do
--        putStrLn "Got the following JSON errors:"
--        mapM_ print esJ
--        putStrLn "Got the following YAML errors:"
--        mapM_ print esY
--  pure ()
--
--batchJSONs :: [A.Value] -> IO ()
--batchJSONs vs = do
--  LB.putStrLn $ A.encode $ A.Array $ V.fromList vs
--
--batchYAMLs :: [A.Value] -> IO ()
--batchYAMLs vs = do
--  LB.putStrLn $ Y.encodeValue vs
