{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AppMain where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import           Data.String (fromString)
import           Katip
import           Network.HTTP.Client hiding (withConnection)
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Random.MWC
import           Text.Printf
------------------------------------------------------------------------------
import           Commands.Cut
import           Commands.CombineSigs
import           Commands.GenTx
import           Commands.Keygen
import           Commands.ListKeys
import           Commands.Local
import           Commands.Mempool
import           Commands.Poll
import           Commands.Send
import           Commands.Sign
import           Commands.Verify
import           Commands.WalletSign
import           Types.Env
------------------------------------------------------------------------------

appMain :: IO ()
appMain = do
    Args c severity verbosity mcf <- execParser opts
    mgr <- newManager tlsManagerSettings

    s1 <- liftIO $ mkHandleScribe ColorIfTerminal stderr
      (permitItem severity) verbosity
    le <- liftIO $ registerScribe "stderr" s1 defaultScribeSettings
      =<< initLogEnv "myapp" "production"

    logLE le DebugS $ fromStr $ printf "Logging with severity %s, verbosity %s"
      (show severity) (show verbosity)
    rand <- createSystemRandom

    cf <- maybe (getXdgDirectory XdgConfig ("kda" </> "config.json")) pure mcf
    logLE le DebugS $ logStr $ "Loading config from " <> cf
    configExists <- doesFileExist cf
    ecd <- if configExists
             then eitherDecodeFileStrict' cf
             else pure $ Right def
    cd <- case ecd of
      Left e -> error (printf "Error parsing %s\n%s" cf e)
      Right cd -> pure cd

    logLE le DebugS $ logStr $ "Loaded config: " <> show cd
    let theEnv = Env mgr le cd rand
    case c of
      Cut hp ma mn -> cutCommand theEnv hp ma mn
      CombineSigs files -> combineSigsCommand theEnv files
      GenTx args -> genTxCommand theEnv args
      Keygen keyType -> keygenCommand keyType
      ListKeys kf ind -> listKeysCommand kf ind
      Local args -> localCommand theEnv args
      Mempool hp cid ma mn -> mempoolCommand hp ma mn cid
      Poll args -> pollCommand theEnv args
      Send args -> sendCommand theEnv args
      Sign args -> signCommand args
      Verify args -> verifyCommand args
      WalletSign args -> walletSignCommand theEnv args

  where
    opts = info (envP <**> helper) $ mconcat
      [ fullDesc
      , header "kda - Command line tool for interacting with the Kadena blockchain"
      , footerDoc (Just theFooter)
      ]
    theFooter = fromString $ unlines
      [ "Run the following command to enable tab completion:"
      , ""
      , "source <(kda --bash-completion-script `which kda`)"
      ]

