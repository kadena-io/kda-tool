{-# LANGUAGE OverloadedStrings #-}

module TemplateSpec where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Either
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Test.Hspec
import           Text.Mustache
import           Text.Printf
------------------------------------------------------------------------------
import           Commands.GenTx
import           TxTemplate
------------------------------------------------------------------------------

templateSpec :: Spec
templateSpec = do
    it "data reader doesn't have decimal imprecision" $ do
      readVars "foo: 0.0000000001" `shouldBe` (Right val)
    it "enforceEqualArrayLens returns Just 1 for single element vars" $ do
      eealTest varMap `shouldBe` Right (Just 1)
    it "fillValueVars drops outer array for single element vars" $ do
      fillValueVars fooTmpl varMap `shouldBe` Right [fooFilledText]
  where
    val = M.singleton "foo" (Number 0.0000000001)

fooTmplText :: String -> String -> Text
fooTmplText hole1 hole2 = T.unlines
  [ "someText: |-"
  , T.pack $ printf "  This is a %s test" hole1
  , T.pack $ printf "num: %s" hole2
  ]

fooTmpl :: Template
fooTmpl = fst $ fromRight (error "fooTmpl error") $
  parseAndGetVars (fooTmplText "{{{foo}}}" "{{{bar}}}")

fillValArr :: [Text]
fillValArr = ["alpha", "bravo"]

fillValNum :: Scientific
fillValNum = 5.00000000001

varMap :: Map Text Value
varMap = M.fromList
    [ ("foo", mkArr [mkArr $ map String fillValArr])
    , ("bar", Number fillValNum)
    ]
  where
    mkArr = Array . V.fromList

fooFilledText :: Text
fooFilledText = fooTmplText (show fillValArr) (show fillValNum)

eealTest :: Map Text Value -> Either [String] (Maybe Int)
eealTest vm = enforceEqualArrayLens vs
  where
    vs = snd $ partitionEithers $ map parseValueValue (M.toList vm)
