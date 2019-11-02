module Data.OpenAPI
  ( decodeFileAs
  ) where

import           Data.Aeson             (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy   as B
import           Data.OpenAPI.V3.Schema
import           Data.Text

decodeFileAs :: FromJSON a => FilePath -> IO (Either String a)
decodeFileAs path = eitherDecode <$> B.readFile path

main :: IO ()
main = do
    result <- decodeFileAs "test/fixtures/petstore.json" :: IO (Either String OpenAPI)
    putStrLn . show $ result
