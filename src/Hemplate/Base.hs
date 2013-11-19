{-# LANGUAGE OverloadedStrings #-}

module Hemplate.Base (render, renderHemplate) where

import Data.Bson
import Data.Text

import Hemplate.Eval (parseAndRunFile)

render :: FilePath -> Document -> IO Text
render fp doc = parseAndRunFile fp (doc, Nothing) >>= return . pack

renderHemplate :: FilePath -> FilePath -> Document -> IO Text
renderHemplate mfp cfp doc = do
  core <- parseAndRunFile cfp (doc, Nothing)
  parseAndRunFile mfp (doc `merge` ["yield" =: pack core], Nothing) >>= return . pack

