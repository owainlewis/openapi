{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
import qualified Data.OpenAPI          as O
import qualified Data.OpenAPI.V3.Types as Types
import           Test.Hspec

expectParse :: (Show a1, Show b, Eq a1, Eq b)
  => (a2 -> b)
  -> Either a1 a2
  -> b
  -> Expectation
expectParse f r e = (f <$> r) `shouldBe` Right(e)

main :: IO ()
main = hspec $ do
  describe "Info object" $ do
    it "contains all fields" $ do
      result <- O.decodeFileAs "test/fixtures/info.json" :: IO (Either String Types.Info)
      expectParse Types.title result "Sample Pet Store App"
