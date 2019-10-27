{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
import qualified Data.OpenAPI          as O
import qualified Data.OpenAPI.V3.Types as Types
import           Data.Text
import           Test.Hspec

-- Utility for making tests simpler to write using mandatory values
expectParse :: (Show a1, Show b, Eq a1, Eq b)
  => (a2 -> b)
  -> Either a1 a2
  -> b
  -> Expectation
expectParse f r e = (f <$> r) `shouldBe` Right(e)

-- Utility for making tests simpler to write using optional values
expectParseMaybe :: (Show a, Show b, Eq a, Eq b)
  => (c -> Maybe b)
  -> Either a c
  -> b
  -> Expectation
expectParseMaybe f r e = (f <$> r) `shouldBe` (Right. Just $ e)

main :: IO ()
main = hspec $ do
  describe "Info object" $ do
    it "contains all fields" $ do
      result <- O.decodeFileAs "test/fixtures/info.json" :: IO (Either String Types.Info)
      expectParse Types.title result
        "Sample Pet Store App"
      expectParseMaybe (Types.description :: Types.Info -> Maybe Text) result
        "This is a sample server for a pet store."
