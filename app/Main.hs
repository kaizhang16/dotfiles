{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text   as T
import qualified Dotfiles    as D
import           Text.Printf (printf)
import qualified Turtle      as TT

main :: IO ()
main = do
  maybeOSType <- D.uname
  case maybeOSType of
    Nothing -> TT.die ((T.pack . D.errorFormat) "No OS type found")
    Just osType -> do
      templatesDir <- TT.options "Deploy dotfiles to home directory" parser
      maybeTemplates <- D.getTemplates templatesDir
      case maybeTemplates of
        Just templates -> do
          _ <- mapM_ (D.link osType) templates
          printf (D.infoFormat "Succeed.\n")
        Nothing -> TT.die ((T.pack . D.errorFormat) "No template found")

parser :: TT.Parser TT.FilePath
parser = TT.argPath "templates_dir" "The templates directory"
