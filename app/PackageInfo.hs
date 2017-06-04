-- | A whole bunch of ugly web screen scraping. It would be great if
-- Pursuit had a JSON API and Github's API didn't have ridiculously
-- low rate limits for unauthenticated requests.
{-# LANGUAGE OverloadedStrings #-}
module PackageInfo
  ( lookupPackage
  , getMasterCommit
  , Package(..)
  ) where

import           Network.HTTP.Simple
import           Text.HTML.DOM       (sinkDoc)
import           Text.XML.Cursor

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

import           Data.Monoid         ((<>))

data Package = Package Text [Text]

noDep :: Text -> Package
noDep url = Package url []

lookupPackage :: Text -- ^ package name
              -> IO Package -- ^ repo URL, dependencies

-- Some hard-coded hacks for packages that don't appear on Pursuit. It
-- would be great to get rid of these.
lookupPackage "purescript-dom-indexed" = pure (noDep "https://github.com/slamdata/purescript-dom-indexed")
lookupPackage "purescript-fork" = pure (noDep "https://github.com/slamdata/purescript-fork")

lookupPackage name = do
  let url = "https://pursuit.purescript.org/packages/" <> name
  req <- parseRequest (T.unpack url)
  doc <- httpSink req $ const sinkDoc
  let cursor = fromDocument doc
      repos = cursor
          $// element "dt"
          >=> hasContent "Repository"
          >=> followingSibling
          &// element "a"
          >=> attribute "href"

  repo <-
    case repos of
      []  -> error (T.unpack $ "Error cloning repo from " <> url <> ", does it exist?")
      x:_ -> pure x

  let deps = cursor
         $// element "a"
         >=> attributeIs "class" "deplink__link"
         &// content
  pure (Package repo deps)

hasContent :: Text -> Axis
hasContent t c
  | T.strip (T.concat (c $// content)) == t = [c]
  | otherwise = []

-- | Get the commit SHA for the master branch
--
-- Technically will take whatever is the displayed branch on the
-- Github UI
getMasterCommit :: Text -- ^ repo URL
                -> IO Text
getMasterCommit repo = do
  req <- parseRequest (T.unpack repo)
  res <- httpSink req (const sinkDoc)
  let cursor = fromDocument res
      oldStyle = cursor
             $// element "a"
             >=> attributeIs "class" "commit-tease-sha"
             >=> attribute "href"
      newStyle = cursor
             $// element "include-fragment"
             >=> attributeIs "class" "commit-tease commit-loader"
             >=> attribute "src"
  case oldStyle <> newStyle of
    [x] -> pure (T.reverse (T.takeWhile (/= '/') (T.reverse x)))
    _   -> error (T.unpack $ "Could not find commit from " <> repo)
