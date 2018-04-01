{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text   as T
import qualified Dotfiles    as D
import           Text.Printf (printf)
import qualified Turtle      as TT

main :: IO ()
main = do
  maybeTemplates <- D.getTemplates
  case maybeTemplates of
    Just templates -> do
      _ <- mapM_ D.link templates
      printf (D.infoFormat "Succeed.\n")
    Nothing -> TT.die ((T.pack . D.errorFormat) "No template found")
