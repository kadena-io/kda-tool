module Main where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           TemplateSpec
------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "TemplateSpec" templateSpec

