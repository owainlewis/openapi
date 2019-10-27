module TestUtil
  (expectParse, expectParseMaybe)
  where

import           Test.Hspec (Expectation, shouldBe)

-- Utility for making tests simpler to write using mandatory values
expectParse :: (Show a, Show b, Eq a, Eq b)
  => (c -> b)
  -> Either a c
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
