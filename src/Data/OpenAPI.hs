module Data.OpenAPI
  ( readOpenAPI
  , main
  ) where

import           Data.Aeson            (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy  as B
import           Data.OpenAPI.V3.Types

readOpenAPI :: FromJSON a => FilePath -> IO (Either String a)
readOpenAPI path = eitherDecode <$> B.readFile path

main :: IO ()
main = do
    result <- readOpenAPI "test/fixtures/path_item.json" :: IO (Either String PathItem)
    putStrLn . show $ result
    return ()
