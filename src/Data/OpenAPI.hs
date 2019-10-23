module Data.OpenAPI
  ( readOpenAPI
  ) where

import           Data.Aeson            (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy  as B
import           Data.OpenAPI.V3.Types

readOpenAPI :: FromJSON a => FilePath -> IO (Either String a)
readOpenAPI path = eitherDecode <$> B.readFile path

main :: IO ()
main = do
    result <- readOpenAPI "test/fixtures/info.json" :: IO (Either String Info)
    putStrLn . show $ result
    return ()
