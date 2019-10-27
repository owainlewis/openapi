{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Data.OpenAPI.V3.Schema where

import           Data.Aeson
import           Data.Data    (Data (..), Typeable)
import           Data.Text    (Text)
import           GHC.Generics

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ <710
{-# LANGUAGE PolyKinds #-}
#endif
#include "overlapping-compat.h"
module Data.Swagger.Internal where

import Prelude ()
import Prelude.Compat

import           Control.Lens             ((&), (.~), (?~))
import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson.Types         as JSON
import           Data.Data                (Data(..), Typeable, mkConstr, mkDataType, Fixity(..), Constr, DataType, constrIndex)
import           Data.Hashable            (Hashable)
import qualified Data.HashMap.Strict      as HashMap
import           Data.HashSet.InsOrd      (InsOrdHashSet)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid              (Monoid (..))
import           Data.Semigroup.Compat    (Semigroup (..))
import           Data.Scientific          (Scientific)
import           Data.String              (IsString(..))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)
import           Network.Socket           (HostName, PortNumber)
import           Network.HTTP.Media       (MediaType)
import           Text.Read                (readMaybe)

import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap

import Generics.SOP.TH                  (deriveGeneric)
import Data.Swagger.Internal.AesonUtils (sopSwaggerGenericToJSON
                                        ,sopSwaggerGenericToJSONWithOpts
                                        ,sopSwaggerGenericParseJSON
                                        ,HasSwaggerAesonOptions(..)
                                        ,AesonDefaultValue(..)
                                        ,mkSwaggerAesonOptions
                                        ,saoAdditionalPairs
                                        ,saoSubObject)
import Data.Swagger.Internal.Utils

#if MIN_VERSION_aeson(0,10,0)
import Data.Swagger.Internal.AesonUtils (sopSwaggerGenericToEncoding)
#define DEFINE_TOENCODING toEncoding = sopSwaggerGenericToEncoding
#else
#define DEFINE_TOENCODING
#endif

-- $setup
-- >>> :seti -XDataKinds
-- >>> import Data.Aeson

-- | A list of definitions that can be used in references.
type Definitions = InsOrdHashMap Text

-- | This is the root document object for the API specification.
data Swagger = Swagger
  { -- | Provides metadata about the API.
    -- The metadata can be used by the clients if needed.
    _swaggerInfo :: Info

    -- | The host (name or ip) serving the API. It MAY include a port.
    -- If the host is not included, the host serving the documentation is to be used (including the port).
  , _swaggerHost :: Maybe Host

    -- | The base path on which the API is served, which is relative to the host.
    -- If it is not included, the API is served directly under the host.
    -- The value MUST start with a leading slash (/).
  , _swaggerBasePath :: Maybe FilePath

    -- | The transfer protocol of the API.
    -- If the schemes is not included, the default scheme to be used is the one used to access the Swagger definition itself.
  , _swaggerSchemes :: Maybe [Scheme]

    -- | A list of MIME types the APIs can consume.
    -- This is global to all APIs but can be overridden on specific API calls.
  , _swaggerConsumes :: MimeList

    -- | A list of MIME types the APIs can produce.
    -- This is global to all APIs but can be overridden on specific API calls.
  , _swaggerProduces :: MimeList

    -- | The available paths and operations for the API.
    -- Holds the relative paths to the individual endpoints.
    -- The path is appended to the @'basePath'@ in order to construct the full URL.
  , _swaggerPaths :: InsOrdHashMap FilePath PathItem

    -- | An object to hold data types produced and consumed by operations.
  , _swaggerDefinitions :: Definitions Schema

    -- | An object to hold parameters that can be used across operations.
    -- This property does not define global parameters for all operations.
  , _swaggerParameters :: Definitions Param

    -- | An object to hold responses that can be used across operations.
    -- This property does not define global responses for all operations.
  , _swaggerResponses :: Definitions Response

    -- | Security scheme definitions that can be used across the specification.
  , _swaggerSecurityDefinitions :: Definitions SecurityScheme

    -- | A declaration of which security schemes are applied for the API as a whole.
    -- The list of values describes alternative security schemes that can be used
    -- (that is, there is a logical OR between the security requirements).
    -- Individual operations can override this definition.
  , _swaggerSecurity :: [SecurityRequirement]

    -- | A list of tags used by the specification with additional metadata.
    -- The order of the tags can be used to reflect on their order by the parsing tools.
    -- Not all tags that are used by the Operation Object must be declared.
    -- The tags that are not declared may be organized randomly or based on the tools' logic.
    -- Each tag name in the list MUST be unique.
  , _swaggerTags :: InsOrdHashSet Tag

    -- | Additional external documentation.
  , _swaggerExternalDocs :: Maybe ExternalDocs
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | The object provides metadata about the API.
-- The metadata can be used by the clients if needed,
-- and can be presented in the Swagger-UI for convenience.
data Info = Info
  { _infoTitle :: Text
  , _infoDescription :: Maybe Text
  , _infoTermsOfService :: Maybe Text
  , _infoContact :: Maybe Contact
  , _infoLicense :: Maybe License
  , _infoVersion :: Text
  } deriving (Eq, Show, Generic, Data, Typeable)

data Contact = Contact
  { _contactName  :: Maybe Text
  , _contactUrl   :: Maybe Text
  , _contactEmail :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

data License = License
  {
    _licenseName :: Text
  , _licenseUrl  :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- =======================================================================
-- Generic ToJSON instances
-- =======================================================================

instance ToJSON Contact where
  toEncoding = genericToEncoding defaultOptions

-- =======================================================================
-- Generic FromJSON instances
-- =======================================================================

instance FromJSON Contact
