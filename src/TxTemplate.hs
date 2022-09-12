{-# LANGUAGE TupleSections #-}

module TxTemplate where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as A
import qualified Data.Attoparsec.ByteString as Atto
import           Data.Bifunctor
import           Data.Either
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Vector as V
import qualified Text.Mustache.Types as MU
import           Text.Mustache
import           Text.Mustache.Types
------------------------------------------------------------------------------

data FillFailure
  = FillIncomplete
  | FillErrors [String]
  deriving (Eq,Ord,Show)

prettyFailure :: FillFailure -> String
prettyFailure FillIncomplete = "Fill incomplete.  You need to specify more fields."
prettyFailure (FillErrors es) = unlines $ "Fill had errors:" : es

fillFilenameVars
  :: Template
  -> Map Text A.Value
  -> Either FillFailure [Text]
fillFilenameVars tmpl varMap =
  if any (== A.Null) (M.elems varMap)
    then Left FillIncomplete
    else if (not $ null es)
           then Left $ FillErrors es
           else do
             mint <- first FillErrors $ enforceEqualArrayLens vs
             forM (transposeArrays mint vs) $ \newVs -> do
               pure $ substitute tmpl (MU.Object $ HM.fromList newVs)
  where
    (es,vs) = partitionEithers $ map onlyArrays (M.toList varMap)
    onlyArrays :: (Text, A.Value) -> Either String (Text, MU.Value)
    onlyArrays (k,v) = do
        v2 <- case v of
          A.Array _ -> pure v
          _ -> Left "Filename template values must be arrays"
        pure (k,mFromJSON v2)


fillTextVars
  :: Template
  -> Map Text Text
  -> Either FillFailure [Text]
fillTextVars tmpl varMap =
  if any T.null (M.elems varMap)
    then Left FillIncomplete
    else if (not $ null es)
           then Left $ FillErrors es
           else do
             mint <- first FillErrors $ enforceEqualArrayLens vs
             forM (transposeArrays mint vs) $ \newVs -> do
               pure $ substitute tmpl (MU.Object $ HM.fromList newVs)
  where
    (es,vs) = partitionEithers $ map parseTextValue (M.toList varMap)

fillValueVars
  :: Template
  -> Map Text A.Value
  -> Either FillFailure [Text]
fillValueVars tmpl varMap =
  if any (== A.Null) (M.elems varMap)
    then Left FillIncomplete
    else if (not $ null es)
           then Left $ FillErrors es
           else do
             mint <- first FillErrors $ enforceEqualArrayLens vs
             forM (transposeArrays mint $ map (second (maybe id replicateSingleArr mint)) vs) $ \newVs -> do
               pure $ substituteValue tmpl (MU.Object $ HM.fromList newVs)
  where
    (es,vs) = partitionEithers $ map parseValueValue (M.toList varMap)

transposeArrays :: Maybe Int -> [(Text, MU.Value)] -> [[(Text, MU.Value)]]
transposeArrays Nothing thepairs = [thepairs]
transposeArrays (Just n) thepairs = transpose . map (\(k,vs) -> map (k,) vs) $ go thepairs
  where
    go [] = []
    go ((k,v):ps) = case v of
      MU.Array a -> (k, V.toList a) : go ps
      _ -> (k, replicate n v) : go ps

enforceEqualArrayLens :: [(Text, MU.Value)] -> Either [String] (Maybe Int)
enforceEqualArrayLens = go [] Nothing False . filter isArrWithMultiple
  where
    topErr = "All fields that are arrays must have the same length or be length 1"
    go lenStrs mFirstLen isMismatch [] =
      if isMismatch then Left (topErr : lenStrs) else Right mFirstLen
    go lenStrs mFirstLen isMismatch ((k,v):ps) =
      case v of
        MU.Array a ->
          let len = V.length a
              msg = T.unpack k <> " is an array with length " <> show len
              newMismatch = isMismatch || maybe False (/= len) mFirstLen
          in go (msg : lenStrs) (mFirstLen <|> Just len) newMismatch ps
        _ -> go lenStrs mFirstLen isMismatch ps
    isArrWithMultiple (_,MU.Array a) = V.length a > 1
    isArrWithMultiple _ = False

replicateSingleArr :: Int -> MU.Value -> MU.Value
replicateSingleArr n v@(MU.Array a) = if V.length a == 1 then MU.Array $ V.replicate n (a V.! 0) else v
replicateSingleArr _ v = v

parseTextValue :: (Text, Text) -> Either String (Text, MU.Value)
parseTextValue (k,vt) = do
    let bs = encodeUtf8 vt
        num = A.Number <$> Atto.parseOnly (A.scientific <* Atto.endOfInput) bs
        str = Right $ A.String vt
    v <- first addLoc $ A.eitherDecodeStrict bs <|> num <|> str
    v2 <- case v of
      A.Number _ -> pure $ A.String vt
      A.String _ -> pure v
      A.Array _ -> pure v
      _ -> Left "Template values must be a Number, String, or Array of numbers or strings"
    pure (k,mFromJSON v2)
  where
    addLoc s = unlines [s, "...in field " <> T.unpack k <> ": " <> T.unpack vt]

parseValueValue :: (Text, A.Value) -> Either String (Text, MU.Value)
parseValueValue (k,v) = do
    v2 <- case v of
      A.Number n -> pure $ A.String $ T.pack $ show n
      _ -> pure v
      -- A.String _ -> pure v
      -- A.Array _ -> pure v
      -- _ -> Left "Template values must be a Number, String, or Array of numbers or strings"
    pure (k,mFromJSON v2)

parseValueValue2 :: (Text, A.Value) -> Either String (Text, MU.Value)
parseValueValue2 (k,v) = do
    v2 <- case v of
      A.Array a ->
        if V.length a == 1 then pure (a V.! 0) else pure v
      _ -> pure v
    pure (k,mFromJSON v2)

getMustacheVariables :: STree -> Set [Text]
getMustacheVariables t = go t
  where
    go ns = S.unions $ map doNode ns
    doNode (Variable _ (NamedData ts)) = S.singleton ts
    doNode (Section _ s) = go s
    doNode _ = mempty

getSingletonVariables :: STree -> Either String (Set Text)
getSingletonVariables t = S.fromList <$> sequence (map toSingle vars)
  where
    vars = S.toList $ getMustacheVariables t
    toSingle [a] = Right a
    toSingle _ = Left "Compound/nested template variables not allowed"

parseAndGetVars :: Text -> Either String (Template, Set Text)
parseAndGetVars t = do
  tmpl <- first show (compileTemplate "tx" t)
  vs <- getSingletonVariables $ ast tmpl
  pure (tmpl, vs)

