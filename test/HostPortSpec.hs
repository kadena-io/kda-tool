{-# LANGUAGE OverloadedStrings #-}

module HostPortSpec where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.Hspec
------------------------------------------------------------------------------
import           Types.HostPort
------------------------------------------------------------------------------

hostPortSpec :: Spec
hostPortSpec = do
  describe "HostPort" $ do
    let h = "foo.example.com"
        p = 80
    it "fails properly on invalid port" $
      hostPortFromText (h <> ":a") `shouldBe`
      Left "Error parsing port in: foo.example.com:a"

    -- These two cases are covered by the SchemeHostPort tests below but
    -- including these here for good measure / clarity.
    it "parses without a port" $
      hostPortFromText h `shouldBe` Right (HostPort h Nothing)
    it "parses with a port" $
      hostPortFromText (h <> ":" <> T.pack (show p)) `shouldBe`
      Right (HostPort h (Just p))
  describe "SchemeHostPort" $ do
    mapM_ testShpPair shpPairs
    it "fails properly on invalid scheme" $
      schemeHostPortFromText "file://foo.example.com" `shouldBe`
      Left "Invalid scheme for host file://foo.example.com"

testShpPair :: (Text, SchemeHostPort) -> Spec
testShpPair (t, hp) = do
  it ("parses " <> T.unpack t) $
    schemeHostPortFromText t `shouldBe` Right hp
  it ("renders " <> T.unpack t) $
    schemeHostPortToText hp `shouldBe` t

shpPairs :: [(Text, SchemeHostPort)]
shpPairs =
  [ ("foo.example.com", SchemeHostPort Nothing $ HostPort "foo.example.com" Nothing)
  , ("foo.example.com:80", SchemeHostPort Nothing $ HostPort "foo.example.com" (Just 80))
  , ("http://foo.example.com", SchemeHostPort (Just Http) $ HostPort "foo.example.com" Nothing)
  , ("http://foo.example.com:1848", SchemeHostPort (Just Http) $ HostPort "foo.example.com" (Just 1848))
  , ("https://foo.example.com", SchemeHostPort (Just Https) $ HostPort "foo.example.com" Nothing)
  , ("https://foo.example.com:4443", SchemeHostPort (Just Https) $ HostPort "foo.example.com" (Just 4443))
  ]
