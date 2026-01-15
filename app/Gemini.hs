module Gemini
    ( responsePath
    , errorPath
    , request
    ) where

import Prelude hiding (lookup)
import Text.Printf (printf)

apiUrl :: String
apiUrl = "https://generativelanguage.googleapis.com/v1beta/models/gemini-3-flash-preview:generateContent"

request :: String -> String -> [String]
request prompt key = [
    apiUrl,
    "--silent",
    "-H", "x-goog-api-key: " ++ key,
    "-H", "Content-Type: application/json",
    "-X", "POST",
    "-d", printf "{\"contents\": [ { \"parts\": [ { \"text\": \"%s\" } ] } ] }" prompt
  ]

responsePath :: [String]
responsePath = [ "candidates", "0", "content", "parts", "0", "text" ]

errorPath :: [String]
errorPath = [ "error", "message" ]

