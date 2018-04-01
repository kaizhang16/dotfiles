{-# LANGUAGE OverloadedStrings #-}

module Dotfiles
  ( errorFormat
  , getTemplates
  , infoFormat
  , link
  , uname
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
  , templateType :: TemplateType
  } deriving (Show)

data TemplateType
  = Linux
  | Darwin
  | Common
  deriving (Eq, Read, Show)

getTemplates :: TT.FilePath -> IO (Maybe [Template])
getTemplates templatesDir =
  case (T.last . filePath2Text) templatesDir of
    '/' -> getTemplates' templatesDir
    _ ->
      getTemplates' (text2FilePath (T.append (filePath2Text templatesDir) "/"))
  where
    getTemplates' templatesDir' = do
      files <- TT.fold (TT.lstree templatesDir') F.list
      files' <- M.filterM TT.testfile files
      home <- TT.home
      return $ mapM (src2Template templatesDir' home) files'

link :: TemplateType -> Template -> IO ()
link osType t
  | (tType == osType) || (tType == Common) = do
    src <- (TT.realpath . templateSrc) t
    mkDestParentDir t
    TT.stdout $
      TT.inproc
        "ln"
        ["-sf", filePath2Text src, (filePath2Text . templateDest) t]
        TT.empty
    printf
      (infoFormat "%s -> %s\n")
      (filePath2Text src)
      ((filePath2Text . templateDest) t)
  | otherwise = return ()
  where
    tType = templateType t

mkDestParentDir :: Template -> IO ()
mkDestParentDir t = do
  ok <- TT.testdir destParent
  if ok
    then return ()
    else TT.mktree destParent
  where
    destParent = (TT.parent . templateDest) t

src2Template :: TT.FilePath -> TT.FilePath -> TT.FilePath -> Maybe Template
src2Template templatesDir home src =
  case T.stripPrefix (filePath2Text templatesDir) (filePath2Text src) of
    Nothing -> Nothing
    Just file ->
      case normalizeFile templateType' file of
        Nothing -> Nothing
        Just file' ->
          Just
            Template
              { templateSrc = src
              , templateDest =
                  text2FilePath $ T.concat [filePath2Text home, "/.", file']
              , templateType = templateType'
              }
  where
    templateType' = getTemplateType src
    normalizeFile Linux  = T.stripSuffix "_linux"
    normalizeFile Darwin = T.stripSuffix "_darwin"
    normalizeFile _      = Just

getTemplateType :: TT.FilePath -> TemplateType
getTemplateType src
  | T.isSuffixOf "_linux" src' = Linux
  | T.isSuffixOf "_darwin" src' = Darwin
  | otherwise = Common
  where
    src' = filePath2Text src

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

uname :: IO (Maybe TemplateType)
uname = do
  osType <- TT.fold (TT.inproc "uname" [] TT.empty) F.list
  case length osType of
    1 -> return $ (Just . read . T.unpack . TT.lineToText . head) osType
    _ -> return Nothing
