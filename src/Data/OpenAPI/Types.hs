{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.OpenAPI.Types
  ( OpenAPI
  , Info
  ) where

import GHC.Generics
import Data.Aeson

import qualified Data.Text as T

data OpenAPI = OpenAPI {
  openapi :: T.Text
} deriving (Generic, Show)

data Info = Info {
  title       :: T.Text,
  description :: Maybe T.Text
} deriving (Generic, Show)

instance ToJSON Info where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Info
