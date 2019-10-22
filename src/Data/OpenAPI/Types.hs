{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.OpenAPI.Types where

import GHC.Generics

import qualified Data.Text as T

data OpenAPI = OpenAPI {
  openapi :: T.Text
} deriving (Generic, Show)

data Info = Info {
  title       :: T.Text,
  description :: Maybe T.Text
} deriving (Generic, Show)
