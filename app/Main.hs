module Main where

import Control.Monad (unless)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import System.Directory (findExecutable)
import System.Exit (die)
import System.Process (readProcess)
import Text.Printf (printf)

generatePrompt :: String -> String
generatePrompt =
  printf
    "You are a command line assistant that can help users with their tasks.\n\
    \User wants assistance with the following command:\n\n\
    \%s\n\n\
    \Respond with a command that can be used to achieve the desired result.\n\
    \Command should be suitable for Linux-based OS.\n\
    \Output only the command, do not include any additional text. \n\
    \Do not include any quotes or backticks in the output.\n"

processCommand :: String -> IO String
processCommand cmd = do
  let trimmed = generatePrompt . trim $ cmd
  readProcess "claude" ["-p", trimmed] ""
 where
  trim :: String -> String
  trim = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
  params <- getContents
  claudeInstalled <- findExecutable "claude"
  unless (isJust claudeInstalled) $
    die "Error: 'claude' not found in PATH."
  result <- processCommand params

  putStr result
