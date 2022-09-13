{-# LANGUAGE DeriveGeneric #-}

module Commands.Cut
  ( cutCommand
  ) where

------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainwebMeta
import           Chainweb.Api.PactCommand
import           Chainweb.Api.Transaction
import           Control.Error
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as LB
import           Data.Function
import           Data.List
import qualified Data.List.NonEmpty as NE
import           Data.Ord
import           Data.String.Conv
import qualified Data.Text.IO as T
import           Katip
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           System.Exit
import           Text.Printf
------------------------------------------------------------------------------
import           Types.Env
import           Types.HostPort
import           Types.Node
import           Utils
------------------------------------------------------------------------------

cutCommand :: HostPort -> IO ()
cutCommand hp = do
  en <- getNode hp
  case en of
    Left er -> putStrLn er >> exitFailure
    Right n -> do
      resp <- nodeGetCut n
      case statusCode $ responseStatus resp of
        200 -> LB.putStrLn $ responseBody resp

