{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.OpenAPI.V3.TypesSpec (tests) where

import qualified Data.OpenAPI          as O
import qualified Data.OpenAPI.V3.Types as Types
import           Data.Text
import           Test.Hspec

import           TestUtil

tests :: IO ()
tests = hspec $ do
  describe "Info object" $ do
    it "contains all fields" $ do
      result <- O.decodeFileAs "test/fixtures/info.json" :: IO (Either String Types.Info)
      expectParse Types.title result
        "Sample Pet Store App"
      expectParseMaybe (Types.description :: Types.Info -> Maybe Text) result
        "This is a sample server for a pet store."
