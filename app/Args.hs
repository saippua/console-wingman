{-# LANGUAGE GADTs #-}
module Args
    ( validateConfig
    , environmentDescriptor
    , parseArgs
    , Config(..)
    , Environment
    ) where

import System.Exit (die)
import Lib

data Environment = SH | BASH | ZSH | CMD | PWSH deriving (Show, Eq)
environmentDescriptor :: Maybe Environment -> String
environmentDescriptor e = case e of
  Just SH -> "Command should be suitable for Linux-based OS running sh shell."
  Just BASH -> "Command should be suitable for Linux-based OS in bash shell."
  Just ZSH -> "Command should be suitable for Linux-based OS with zsh shell."
  Just CMD -> "Command should be suitable for Windows OS with CMD prompt."
  Just PWSH -> "Command should be suitable for Windows OS with powershell."
  Nothing -> ""



data Config where
  Config :: {environment :: Maybe Environment} -> Config
  deriving (Show)

parseArgs :: [String] -> Config
parseArgs args = go args (Config Nothing)
  where
    go [] config = config
    go (arg:rest) config = case arg of
      -- Environments
      "--sh"    -> go rest (config { environment = Just SH })
      "--bash"  -> go rest (config { environment = Just BASH })
      "--zsh"   -> go rest (config { environment = Just ZSH })
      "--cmd"   -> go rest (config { environment = Just CMD })
      "--pwsh"  -> go rest (config { environment = Just PWSH })
      _         -> go rest config -- Unrecognized flag does nothing


validateConfig :: Config -> IO ()
validateConfig config = do
  case environment config of
    Nothing -> die $ errorString "Environment not specified! Use --sh, --bash, --zsh, --cmd, or --pwsh"
    Just _ -> return ()
