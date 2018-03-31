{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Foldl      as Fold
import           Control.Monad
import           System.Environment
import qualified Turtle             as Turtle

main :: IO ()
main = do
  ts <- getTemplates
  print ts
  home <- lookupEnv "HOME"
  print home

templatesDir :: Turtle.FilePath
templatesDir = "templates/"

getTemplates :: IO [Turtle.FilePath]
getTemplates = do
  files <- Turtle.fold (Turtle.lstree templatesDir) Fold.list
  filterM Turtle.testfile files

src2Dest :: Turtle.FilePath -> Turtle.FilePath -> Maybe Turtle.FilePath
src2Dest src home =
  case (Turtle.stripPrefix templatesDir src) of
    Just file -> Just $ Turtle.concat ["."]
    _         -> Nothing
