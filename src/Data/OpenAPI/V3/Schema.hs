{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Data.OpenAPI.V3.Schema where

import           Data.Aeson
import           Data.Char    (isUpper, toLower)
import           Data.Data    (Data (..), Typeable)
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Text    (Text)
import           GHC.Generics

-- This is the root document object of the OpenAPI document.
data OpenAPI = OpenAPI {
    _openAPI :: Text
  , _info    :: Info
  } deriving (Eq, Show, Generic, Data, Typeable)

-- The object provides metadata about the API.
-- The metadata MAY be used by the clients if needed, and MAY be presented in
-- editing or documentation generation tools for convenience.
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

-- An object representing a Server.
data Server = Server {
    _serverUrl         :: Text
  , _serverDescription :: Maybe Text
  , _serverVariables   :: Map Text ServerVariable
  } deriving (Eq, Show, Generic, Data, Typeable)

data ServerVariable = ServerVariable {
    _serverVariableEnum        :: [Text]
  , _serverVariableDefault     :: Text
  , _serverVariableDescription :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

newtype URL = URL { getUrl :: Text } deriving (Eq, Ord, Show, Generic, Data, Typeable)

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

jsonPrefix :: String -> Options
jsonPrefix prefix = defaultOptions {
    fieldLabelModifier      = modifier . drop 1
  , constructorTagModifier  = modifier
  , sumEncoding             = ObjectWithSingleField
  , omitNothingFields       = True
  }
  where
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

instance FromJSON Contact
  parseJSON = genericParseJSON (jsonPrefix "Contact")
instance FromJSON License
  parseJSON = genericParseJSON (jsonPrefix "License")
instance FromJSON Info where
  parseJSON = genericParseJSON (jsonPrefix "Info")
