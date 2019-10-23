{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Data.OpenAPI.V3.Types
  ( OpenAPI
  , Info
  ) where

import           Data.Aeson
import           GHC.Generics

import qualified Data.Text    as T

data OpenAPI = OpenAPI {
  openapi :: T.Text
} deriving (Generic, Show)

data Info = Info {
    title          :: T.Text
  , description    :: Maybe T.Text
  , termsOfService :: Maybe T.Text
  , contact        :: Maybe Contact
  , license        :: Maybe License
  , version        :: Maybe T.Text
} deriving (Eq, Generic, Show)

instance ToJSON Info where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Info

data Contact = Contact {
    name  :: Maybe T.Text
  , url   :: Maybe T.Text
  , email :: Maybe T.Text
} deriving (Eq, Generic, Show)

instance ToJSON Contact where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Contact

data License = License {
    name :: Maybe T.Text
  , url  :: Maybe T.Text
} deriving (Eq, Generic, Show)

instance ToJSON License where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON License
