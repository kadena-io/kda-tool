{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Commands.CombineSigs
  ( combineSigsCommand
  ) where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad
import           Data.Bifunctor
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Kadena.SigningTypes
import           Katip
import           Pact.Types.Command
import           Pact.Types.SigData
import           System.FilePath
import           System.IO
import           Text.Printf
------------------------------------------------------------------------------
import           Types.Encoding
import           Types.Env
import           Utils
------------------------------------------------------------------------------

combineSigsCommand :: Env -> [FilePath] -> Bool -> IO ()
combineSigsCommand env files legacy = do
  when (not $ all hasYamlExtension files) $
    error "Error: combine-sigs files must have a .yaml extension"

  epairs <- mapM readCommandFile files
  case partitionEithers epairs of
    ([],ps) -> do
      mfiles <- mapM (writeCombined legacy) $ M.toList $ M.unionsWith (++) $ map mkCmdMap ps
      printf "Wrote the following files: %s\n" (intercalate ", " $ catMaybes mfiles)
    (es, _) -> do
      printf "Encountered errors in the following files: %s\n" (intercalate ", " $ map fst es)
      logEnv env DebugS $ logStr $ unlines $ map (\(fp,e) -> fp <> ": " <> e) es

mkCmdMap :: (FilePath, CommandSigData) -> Map Text [(FilePath, SignatureList)]
mkCmdMap (fp, csd) = M.singleton (_csd_cmd csd) [(fp, _csd_sigs csd)]

writeCombined :: Bool -> (Text, [(FilePath, SignatureList)]) -> IO (Maybe FilePath)
writeCombined _ (_, []) = pure Nothing
writeCombined legacy (cmd, fsigs@(p:ps)) = do
  let (fname,sigs) = case ps of
        [] -> (dropExtension $ fst p, snd p)
        _ ->
          let fp = intercalate "-" (sort $ map (dropExtension . takeFileName . fst) fsigs)
          in (fp, combineSigs $ map snd fsigs)
  fp <- saveCommandSigData fname (CommandSigData sigs cmd) legacy
  pure $ Just fp

combineSigs :: [SignatureList] -> SignatureList
combineSigs [] = SignatureList []
combineSigs lists@(s:_) = SignatureList $ map f $ unSignatureList s
  where
    combined = M.unionsWith  mplus $ map sigListToMap lists
    f (CSDSigner pubkey _) = CSDSigner pubkey $ join $ M.lookup pubkey combined

sigListToMap :: SignatureList -> Map PublicKeyHex (Maybe UserSig)
sigListToMap (SignatureList sigs) = M.fromList (map (\(CSDSigner k s) -> (k,s)) sigs)

readCommandFile :: FilePath -> IO (Either (FilePath, String) (FilePath, CommandSigData))
readCommandFile fp = do
  h <- openFile fp ReadMode
  rawbs <- readAsEncoding Yaml h
  hClose h
  let eres = decodeCmdYaml rawbs
  pure $ bimap (\e -> (fp, e)) (fp,) eres
