{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AppMain where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import           Katip
import           Network.HTTP.Client hiding (withConnection)
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           Options.Applicative.Help.Pretty hiding ((</>))
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
import           Commands.WalletSign
import           Types.Env
------------------------------------------------------------------------------

appMain :: IO ()
appMain = do
    Args c sev mcf <- execParser opts
    let verbosity = V2
    mgr <- newManager tlsManagerSettings

    s1 <- liftIO $ mkHandleScribe ColorIfTerminal stderr (permitItem sev) verbosity
    le <- liftIO $ registerScribe "stderr" s1 defaultScribeSettings
      =<< initLogEnv "myapp" "production"

    logLE le DebugS $ logStr $ "Logging with severity " <> show sev
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
      Cut hp -> cutCommand hp
      CombineSigs files -> combineSigsCommand theEnv files
      Tx args -> txCommand theEnv args
      GenTx args -> genTxCommand args
      Keygen keyType -> keygenCommand keyType
      ListKeys kf ind -> listKeysCommand kf ind
      Local args -> localCommand theEnv args
      Mempool hp net cid -> mempoolCommand hp net cid
      Poll args -> pollCommand theEnv args
      Send args -> sendCommand theEnv args
      Sign args -> signCommand args
      Quicksign args -> quicksignCommand theEnv args

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

