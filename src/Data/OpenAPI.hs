module Data.OpenAPI
  (
  )where

import Data.OpenAPI.Types

import Data.Aeson
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    info <- B.readFile "test/fixtures/info.json"
    let result = eitherDecode info :: (Either String Info)
    putStrLn . show $ result
    return ()
