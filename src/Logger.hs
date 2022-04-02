{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Logger
  ( AppLogger
  , initLogger
  , getLogMessages
  , logIO
  , Severity(..)
  , logStr
  , showLS
  , mkReflexScribe
  ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.IO as LT
import           Data.Time
import           System.IO
import           Katip
------------------------------------------------------------------------------

data AppLogger = AppLogger
  { _appLoggerHandle :: Handle
  , _appLoggerSeverity :: Severity
  , _appLoggerRef :: IORef LogBuffer
  }

getLogMessages :: AppLogger -> IO (Map UTCTime LT.Text)
getLogMessages al = do
  LogBuffer m <- readIORef $ _appLoggerRef al
  pure m

logIO :: AppLogger -> Severity -> LogStr -> IO ()
logIO (AppLogger h lsev ref) sev msg = when (sev >= lsev) $ do
  t <- getCurrentTime
  let tstr = ls $ formatTime defaultTimeLocale "%F %T" t
      fullMsg = LT.toLazyText $ unLogStr $ tstr <> " " <> msg
  LT.hPutStrLn h fullMsg
  atomicModifyIORef' ref (\lb -> (addToBuffer t fullMsg lb, ()))

initLogger :: Severity -> IO AppLogger
initLogger sev = do
  h <- openFile "out.log" AppendMode
  hSetBuffering h NoBuffering
  ref <- newIORef (LogBuffer mempty)
  pure $ AppLogger h sev ref

logBufferSize :: Int
logBufferSize = 1000

newtype LogBuffer = LogBuffer (Map UTCTime LT.Text)
  deriving (Eq,Ord)

addToBuffer :: UTCTime -> LT.Text -> LogBuffer -> LogBuffer
addToBuffer t msg (LogBuffer m) = LogBuffer m3
  where
    m2 = M.insert t msg m
    m3 = if M.size m2 > logBufferSize then M.deleteMin m2 else m2

mkReflexScribe
  :: (Text -> IO ())
  -> Bool -- ^ Whether or not to colorize the output
  -> PermitFunc
  -> Verbosity
  -> IO Scribe
mkReflexScribe fireEvent colorize permitF verb = do
    let logger i@Item{..} = do
          let msg = LT.toLazyText $ shortFormat colorize verb i
          fireEvent $ LT.toStrict msg
    return $ Scribe logger (pure ()) permitF

shortFormat :: LogItem a => ItemFormatter a
shortFormat _ _ Item{..} =
    brackets nowStr <>
--    brackets (LT.fromText (renderSeverity' _itemSeverity)) <>
    LT.fromText " " <>
    (unLogStr _itemMessage)
  where
    nowStr = LT.fromText (T.pack $ formatTime defaultTimeLocale "%F %T" _itemTime)
--    renderSeverity' severity = renderSeverity severity

brackets :: LT.Builder -> LT.Builder
brackets m = LT.fromText "[" <> m <> LT.fromText "]"
