{-# LANGUAGE OverloadedStrings #-}
module Main where

import Directory
import System

import Control.Applicative
import Control.Monad.Trans
import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import Text.Templating.Heist

site :: TemplateState IO -> Snap ()
site ts = 
    ifTop (writeBS =<< liftIO (renderTemplate ts "index")) <|>
    template "about" <|>
    template "download" <|>
    template "docs" <|>
    template "contribute" <|>
    template "news" <|>
    fileServe "static"
  where
    template n = dir n (writeBS =<< liftIO (renderTemplate ts n))

main :: IO ()
main = do
    args <- getArgs
    templateState <- loadTemplates "templates"
    httpServe "*" (read $ head args) "achilles"
              (Just "access.log")
              (Just "error.log")
              (site templateState)
