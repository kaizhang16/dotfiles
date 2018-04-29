{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Dotfiles
  ( deploy
  , echoError
  , echoInfo
  , echoWarn
  ) where

import qualified Control.Monad   as M
import           Data.Monoid     ((<>))
import qualified Data.Text       as T
import qualified Filesystem.Path as P
import qualified Shelly          as SH
default (T.Text)

data Template = Template
  { templateSrc  :: SH.FilePath
  , templateDest :: SH.FilePath
  , templateType :: TemplateType
  } deriving (Show)

data TemplateType
  = Linux
  | Darwin
  | Common
  deriving (Eq, Read, Show)

deploy ::
     TemplateType -> SH.FilePath -> SH.FilePath -> [SH.FilePath] -> SH.Sh ()
deploy os templatesPath home = M.mapM_ deploy'
  where
    deploy' src =
      case src2Template src templatesPath home of
        Nothing -> SH.errorExit ("Bad format: " <> SH.toTextIgnore src)
        Just t  -> deploy'' t
    deploy'' t
      | (templateType t == os) || (templateType t == Common) = do
        SH.mkdir_p (P.parent (templateDest t))
        SH.cmd "ln" "-f" (templateSrc t) (templateDest t)
      | otherwise =
        echoWarn ("Ignore " <> (SH.toTextIgnore . templateSrc) t <> ".")

src2Template :: SH.FilePath -> SH.FilePath -> SH.FilePath -> Maybe Template
src2Template src templatesPath home =
  case T.stripPrefix (SH.toTextIgnore templatesPath) (SH.toTextIgnore src) of
    Nothing -> Nothing
    Just src' ->
      case normalizeSrc templateType' (dropExtension src') of
        Nothing -> Nothing
        Just file' ->
          Just
            Template
              { templateSrc = src
              , templateDest =
                  SH.fromText $
                  T.concat [SH.toTextIgnore home, "/.", file', srcExtension]
              , templateType = templateType'
              }
  where
    templateType' = getTemplateType src
    srcExtension =
      case P.extension src of
        Nothing  -> ""
        Just ext -> "." <> ext
    dropExtension = SH.toTextIgnore . P.dropExtension . SH.fromText
    normalizeSrc Linux  = T.stripSuffix "_linux"
    normalizeSrc Darwin = T.stripSuffix "_darwin"
    normalizeSrc _      = Just

getTemplateType :: SH.FilePath -> TemplateType
getTemplateType src
  | T.isSuffixOf "_linux" src' = Linux
  | T.isSuffixOf "_darwin" src' = Darwin
  | otherwise = Common
  where
    src' = SH.toTextIgnore (P.dropExtension src)

red :: T.Text
red = "\x1b[31m"

green :: T.Text
green = "\x1b[32m"

yellow :: T.Text
yellow = "\x1b[33m"

noColor :: T.Text
noColor = "\x1b[0m"

echoError :: T.Text -> SH.Sh ()
echoError msg = SH.echo (red <> msg <> noColor)

echoInfo :: T.Text -> SH.Sh ()
echoInfo msg = SH.echo (green <> msg <> noColor)

echoWarn :: T.Text -> SH.Sh ()
echoWarn msg = SH.echo (yellow <> msg <> noColor)
