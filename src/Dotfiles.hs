{-# LANGUAGE OverloadedStrings #-}

module Dotfiles
  ( errorFormat
  , getTemplates
  , infoFormat
  , link
  ) where

import qualified Control.Foldl as F
import qualified Control.Monad as M
import           Data.Monoid   ((<>))
import qualified Data.String   as S
import qualified Data.Text     as T
import           Text.Printf   (printf)
import qualified Turtle        as TT

data Template = Template
  { templateSrc  :: TT.FilePath
  , templateDest :: TT.FilePath
  } deriving (Show)

templatesDir :: TT.FilePath
templatesDir = "templates/"

getTemplates :: IO (Maybe [Template])
getTemplates = do
  files <- TT.fold (TT.lstree templatesDir) F.list
  files' <- M.filterM TT.testfile files
  home <- TT.home
  return $ mapM (src2Template home) files'

link :: Template -> IO ()
link t = do
  src <- (TT.realpath . templateSrc) t
  TT.stdout $
    TT.inproc
      "ln"
      ["-sf", filePath2Text src, (filePath2Text . templateDest) t]
      TT.empty
  printf
    (infoFormat "%s -> %s\n")
    (filePath2Text src)
    ((filePath2Text . templateDest) t)

src2Template :: TT.FilePath -> TT.FilePath -> Maybe Template
src2Template home src =
  case T.stripPrefix (filePath2Text templatesDir) (filePath2Text src) of
    Just file ->
      Just
        Template
          { templateSrc = src
          , templateDest =
              text2FilePath $ T.concat [filePath2Text home, "/.", file]
          }
    Nothing -> Nothing

filePath2Text :: TT.FilePath -> T.Text
filePath2Text = TT.format TT.fp

text2FilePath :: T.Text -> TT.FilePath
text2FilePath = S.fromString . T.unpack

red :: String
red = "\x1b[31m"

green :: String
green = "\x1b[32m"

noColor :: String
noColor = "\x1b[0m"

infoFormat :: String -> String
infoFormat s = green <> s <> noColor

errorFormat :: String -> String
errorFormat s = red <> s <> noColor
