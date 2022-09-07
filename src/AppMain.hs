{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module AppMain where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Default
import           Data.Either
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.YAML.Aeson as Y
import           Katip
import           Network.HTTP.Client hiding (withConnection)
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           System.Exit
import           System.IO
import           System.Random.MWC
import           Text.Printf
------------------------------------------------------------------------------
import           Commands.Send
import           Types.Env
------------------------------------------------------------------------------

appMain :: IO ()
appMain = do
    Args c sev configFile <- execParser opts
    let verbosity = V2
    mgr <- newManager tlsManagerSettings

    s1 <- liftIO $ mkHandleScribe ColorIfTerminal stderr (permitItem sev) verbosity
    le <- liftIO $ registerScribe "stderr" s1 defaultScribeSettings
      =<< initLogEnv "myapp" "production"

    logLE le InfoS $ logStr $ "Logging with severity " <> show sev
    ecd <- maybe (pure $ Right def) A.eitherDecodeFileStrict' configFile
    cd <- case ecd of
      Left e -> error (printf "Error parsing %s\n%s" (fromJust configFile) e)
      Right cd -> pure cd
    rand <- createSystemRandom
    let theEnv = Env mgr le cd rand
    case c of
      Batch files -> batchCommand files
      Send args -> sendCommand args
      _ -> putStrLn "Not implemented yet" >> exitWith (ExitFailure 1)

  where
    opts = info (envP <**> helper)
      (fullDesc <> header "myapp - Haskell command line app template")

fireNothing :: a -> IO ()
fireNothing _ = pure ()

batchCommand :: [FilePath] -> IO ()
batchCommand files = do
  bss <- mapM LB.readFile files
  case partitionEithers $ A.eitherDecode <$> bss of
    ([],vs) -> batchJSONs vs
    (esJ,_) -> case partitionEithers $ Y.decode1Strict . LB.toStrict <$> bss of
      ([],vs) -> batchYAMLs vs
      (esY,_) -> do
        putStrLn "Got the following JSON errors:"
        mapM_ print esJ
        putStrLn "Got the following YAML errors:"
        mapM_ print esY
  pure ()

batchJSONs :: [A.Value] -> IO ()
batchJSONs vs = do
  LB.putStrLn $ A.encode $ A.Array $ V.fromList vs

batchYAMLs :: [A.Value] -> IO ()
batchYAMLs vs = do
  LB.putStrLn $ Y.encodeValue vs
