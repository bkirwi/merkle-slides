#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Binary
import Data.Functor
import Data.List
import Data.Maybe
import Data.Monoid

import System.Process (readProcess)

import Text.HTML.TagSoup
import Text.Pandoc
import Text.Pandoc.JSON

graphViz :: Block -> IO Block
graphViz (CodeBlock attrs@(ident, classes, pairs) text) | "dot" `elem` classes = do
  let rankdir = fromMaybe "TB" $ lookup "rankdir" pairs
      options =
        [ "-Tsvg"
        , "-Earrowsize=0.8"
        , "-Grankdir=" <> rankdir
        ]
  svg <- readProcess "dot" options text
  let cleaned = unlines $ drop 6 $ lines svg
      rewritten = stripStyle cleaned
  return $ RawBlock (Format "html") rewritten
graphViz x = return x

stripStyle :: String -> String
stripStyle = renderTags . map go . parseTags
  where
    go (TagOpen s a) = TagOpen s $ filter fixAttr a
    go x = x
    fixAttr ("fill", _) = False
    fixAttr ("stroke", _) = False
    fixAttr ("font-family", _) = False
    fixAttr x = True

main :: IO ()
main = toJSONFilter graphViz
