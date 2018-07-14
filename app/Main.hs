{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import           Data.Version        (showVersion)
import qualified Dotfiles            as D
import qualified Options.Applicative as O
import           Paths_dotfiles      (version)
import           Shelly
default (T.Text)

data Command
  = Deploy { templatesDir :: String }
  | Version

parseDeployCommand :: O.Parser Command
parseDeployCommand =
  Deploy <$>
  O.argument O.str (O.metavar "TEMPLATES_DIRECTORY" <> O.help "The templates directory")

parseVersionCommand :: O.Parser Command
parseVersionCommand = pure Version

parseCommand :: O.Parser Command
parseCommand =
  O.subparser $
  O.command
    "deploy"
    (O.info
       (O.helper <*> parseDeployCommand)
       (O.fullDesc <>
        O.progDesc "Deploy the dotfiles from TEMPLATES_DIRECTORY to $HOME")) <>
  O.command
    "version"
    (O.info
       (O.helper <*> parseVersionCommand)
       (O.fullDesc <> O.progDesc "Show version"))

main :: IO ()
main = dotfiles =<< O.execParser opts
  where
    opts =
      O.info
        (O.helper <*> parseCommand)
        (O.header "dotfiles - a configuration files manager")

dotfiles :: Command -> IO ()
dotfiles Version = putStrLn (showVersion version)
dotfiles Deploy {templatesDir = templatesDir'} =
  shelly $
  verbosely $ do
    templatesDir'' <- cmd "realpath" ((fromText . T.pack) templatesDir')
    os <- cmd "uname"
    maybeHome <- get_env "HOME"
    let templatesDir''' = fromText (T.strip templatesDir'' <> "/")
    case maybeHome of
      Nothing -> errorExit "$HOME is empty."
      Just home -> do
        templates <- findWhen test_f templatesDir'''
        D.deploy
          ((read . T.unpack) os)
          templatesDir'''
          (fromText home)
          templates
        D.echoInfo "Succeed."
