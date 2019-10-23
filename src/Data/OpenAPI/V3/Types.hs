{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
module Data.OpenAPI.V3.Types
  ( OpenAPI
  , Info(..)
  , Contact(..)
  , PathItem(..)
  ) where

import           Data.Aeson   (FromJSON (..), ToJSON (..), defaultOptions,
                               genericToEncoding, withObject, (.:), (.:?))
import           GHC.Generics (Generic)

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

-- License information for the exposed API.
-- This object MAY be extended with Specification Extensions.
data License = License {
    name :: Maybe T.Text
  , url  :: Maybe T.Text
} deriving (Eq, Generic, Show)

instance ToJSON License where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON License

-- An object representing a Server.
-- This object MAY be extended with Specification Extensions.
data Server = Server {
    url         :: T.Text
  , description :: Maybe T.Text
  -- variables Map[String, ServerVariable]
} deriving (Eq, Generic, Show)

instance ToJSON Server where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Server

data PathItem = PathItem {
    ref         :: Maybe T.Text
  , summary     :: Maybe T.Text
  , description :: Maybe T.Text
} deriving (Eq, Show)

instance FromJSON PathItem where
  parseJSON = withObject "pathItem" $ \o -> do
    ref <- o .:? "$ref"
    summary  <- o .:? "summary"
    description <- o .:? "description"
    return PathItem{..}
