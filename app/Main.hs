import Data.Aeson
import Data.Char (isSpace)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.ByteString.Lazy.Char8 as BL

import Text.Printf (printf)
import Control.Monad (when)
import Control.Exception (catchJust)

import System.Process (readProcess)
import System.Directory (findExecutable)
import System.Environment (getArgs, getEnv)
import System.Exit (die)
import System.IO.Error (isDoesNotExistError)

import Gemini (request, responsePath, errorPath)
import Lib
import Args

generatePrompt :: String -> Maybe Environment -> String
generatePrompt prompt env =
  printf
    "You are a command line assistant that can help users with their tasks.\n\
    \User wants assistance with the following command:\n\n\
    \%s\n\n\
    \Respond with a command that can be used to achieve the desired result.\n\
    \%s\n\
    \Output only the command, do not include any additional text. \n\
    \Do not include any quotes or backticks in the output.\n" prompt $ environmentDescriptor env


main :: IO ()
main = do

  -- Check that the API key is correctly setup
  apiKey <- catchJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (getEnv "GEMINI_API_KEY")
    (\_ -> die $ errorString "Set Google AI Studio API key environment variable (GEMINI_API_KEY)")

  -- Check that CURL is installed on the PATH
  curlInstalled <- findExecutable "curl"
  when (isNothing curlInstalled) $
    die $ errorString "curl not found on PATH!"

  args <- getArgs
  prompt <- getContents

  let config = parseArgs args
  validateConfig config

  -- putStrLn $ printf "args: %s" $ head args
  -- putStrLn $ printf "prompt: %s" prompt
  -- putStrLn $ printf "env: %s" $ environmentDescriptor $ environment config


  let bigPrompt = generatePrompt (trim prompt) (environment config)
  jsonString <- readProcess "curl" (request bigPrompt apiKey) ""

  let obj = decode $ BL.pack jsonString :: Maybe Value

  when (isNothing obj) $
    die $ errorString $ printf "Invalid JSON:\n%s\n" jsonString

  let val = extractValue responsePath $ fromJust obj

  when (isNothing val) $
    case extractValue errorPath $ fromJust obj of
      Nothing -> die $ errorString $ printf "Invalid JSON:\n%s\n" jsonString
      Just msg -> die $ errorString msg

  putStr $ fromJust val

  where
    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse


