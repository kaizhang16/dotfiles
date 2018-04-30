{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import qualified Dotfiles            as D
import           Options.Applicative
import           Shelly
default (T.Text)

newtype Options = Options
  { templatesPath :: String
  }

options :: Parser Options
options =
  Options <$>
  argument str (metavar "TEMPLATES_PATH" <> help "The templates path")


main :: IO ()
main = dotfiles =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <> progDesc "Deploy the dotfiles from TEMPLATES_PATH to $HOME" <>
         header "dotfiles - a configuration files manager")

dotfiles :: Options -> IO ()
dotfiles (Options templatesPath') =
  shelly $
  verbosely $ do
    templatesPath'' <- cmd "realpath" ((fromText . T.pack) templatesPath')
    os <- cmd "uname"
    maybeHome <- get_env "HOME"

    let templatesPath''' = fromText (T.strip templatesPath'' <> "/")
    case maybeHome of
      Nothing -> errorExit "$HOME is empty."
      Just home -> do
        templates <- findWhen test_f templatesPath'''
        D.deploy
          ((read . T.unpack) os)
          templatesPath'''
          (fromText home)
          templates
        D.echoInfo "Succeed."
