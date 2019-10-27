module Data.OpenAPI
  ( decodeFileAs
  ) where

import           Data.Aeson            (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy  as B
import           Data.OpenAPI.V3.Types (Info (..))
import           Data.Text

decodeFileAs :: FromJSON a => FilePath -> IO (Either String a)
decodeFileAs path = eitherDecode <$> B.readFile path

main :: IO ()
main = do
    result <- decodeFileAs "test/fixtures/info.json" :: IO (Either String Info)
    putStrLn . show $ (description :: Info -> Maybe Text) <$> result
