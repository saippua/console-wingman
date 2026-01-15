module Lib
    ( extractValue
    , splitOn
    , errorString
    ) where

import Data.Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Prelude hiding (lookup)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Printf (printf)

errorString :: String -> String
errorString = printf "\ESC[31m[ERROR]\ESC[0m %s"

--------------------------------------------------------------------

-- Extract a value from JSON given a path like ["user", "name"]
extractValue :: [String] -> Value -> Maybe String
extractValue [] (String v) = Just (T.unpack v)
extractValue [] v = Nothing
extractValue (key:rest) (Object obj) =
    case lookup (fromString key) obj of
        Just v -> extractValue rest v
        Nothing -> Nothing
extractValue (idx:rest) (Array arr) =
    case reads idx :: [(Int, String)] of
        [(i, "")] | i >= 0 && i < V.length arr ->
            extractValue rest (arr V.! i)
        _ -> Nothing
extractValue _ _ = Nothing

--------------------------------------------------------------------

-- Helper function to split strings
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = let (chunk, rest) = break (== c) s
              in chunk : case rest of
                  [] -> []
                  (_:rest') -> splitOn c rest'
