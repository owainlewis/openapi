{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Data.OpenAPI.V3.Schema where

import           Data.Aeson
import           Data.Data    (Data (..), Typeable)
import           Data.Text    (Text)
import           GHC.Generics

data OpenAPI = OpenAPI {
  _openAPI :: Text
  } deriving (Eq, Show, Generic, Data, Typeable)

data Info = Info {
    _infoTitle          :: Text
  , _infoDescription    :: Maybe Text
  , _infoTermsOfService :: Maybe Text
  , _infoContact        :: Maybe Contact
  , _infoLicense        :: Maybe License
  , _infoVersion        :: Text
  } deriving (Eq, Show, Generic, Data, Typeable)

data Contact = Contact {
    _contactName  :: Maybe Text
  , _contactUrl   :: Maybe Text
  , _contactEmail :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

data License = License {
    _licenseName :: Text
  , _licenseUrl  :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- =======================================================================
-- Generic ToJSON instances
-- =======================================================================

instance ToJSON Contact where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON License where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Info where
  toEncoding = genericToEncoding defaultOptions

-- =======================================================================
-- Generic FromJSON instances
-- =======================================================================

instance FromJSON Contact

instance FromJSON License

instance FromJSON Info
