{-# LANGUAGE OverloadedStrings #-}

module TemplateSpec where

------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Map as M
import           Test.Hspec
------------------------------------------------------------------------------
import           Commands.GenTx
------------------------------------------------------------------------------

templateSpec :: Spec
templateSpec = do
    it "data reader doesn't have decimal imprecision" $ do
      readVars "foo: 0.0000000001" `shouldBe` (Right val)
  where
    val = M.singleton "foo" (Number 0.0000000001)

