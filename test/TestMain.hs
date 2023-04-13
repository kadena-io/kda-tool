module Main where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------
import           HostPortSpec
import           TemplateSpec
------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "TemplateSpec" templateSpec
  describe "HostPort" hostPortSpec

