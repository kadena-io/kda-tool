{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AppMain where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import           Data.Maybe
import           Katip
import           Network.HTTP.Client hiding (withConnection)
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           System.Random.MWC
import           Text.Printf
------------------------------------------------------------------------------
import           Types.Env
------------------------------------------------------------------------------

appMain :: IO ()
appMain = do
    Args c sev configFile <- execParser opts
    let verbosity = V2
    mgr <- newManager tlsManagerSettings

    s1 <- liftIO $ mkFileScribe "out.log" (permitItem sev) verbosity
    le <- liftIO $ registerScribe "file" s1 defaultScribeSettings
      =<< initLogEnv "myapp" "production"

    putStrLn $ "Logging with severity " <> show sev
    ecd <- maybe (pure $ Right def) eitherDecodeFileStrict' configFile
    cd <- case ecd of
      Left e -> error (printf "Error parsing %s\n%s" (fromJust configFile) e)
      Right cd -> pure cd
    rand <- createSystemRandom
    let theEnv = Env mgr le cd rand
    case c of
      Alpha -> do
        putStrLn "Running Alpha with config:"
        print (_env_configData theEnv)
      Beta -> do
        putStrLn "Beta not implemented yet"
      Gamma -> do
        putStrLn "Gamma not implemented yet"

  where
    opts = info (envP <**> helper)
      (fullDesc <> header "myapp - Haskell command line app template")

fireNothing :: a -> IO ()
fireNothing _ = pure ()

