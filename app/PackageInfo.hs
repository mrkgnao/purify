-- | A whole bunch of ugly web screen scraping. It would be great if
-- Pursuit had a JSON API and Github's API didn't have ridiculously
-- low rate limits for unauthenticated requests.
{-# LANGUAGE OverloadedStrings #-}
module PackageInfo
  ( lookupPackage
  , getMasterCommit
  , Package(..)
  ) where

import           Data.Text           (Text)
import qualified Data.Text           as T
import           Network.HTTP.Simple
import           Text.HTML.DOM       (sinkDoc)
import           Text.XML.Cursor

data Package = Package String [String]

noDep :: String -> Package
noDep url = Package url []

lookupPackage :: String -- ^ package name
              -> IO Package -- ^ repo URL, dependencies

-- Some hard-coded hacks for packages that don't appear on Pursuit. It
-- would be great to get rid of these.
lookupPackage "purescript-dom-indexed" = pure (noDep "https://github.com/slamdata/purescript-dom-indexed")
lookupPackage "purescript-fork" = pure (noDep "https://github.com/slamdata/purescript-fork")

lookupPackage name = do
  let url = "https://pursuit.purescript.org/packages/" ++ name
  req <- parseRequest url
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
      []  -> error ("Error cloning repo from " ++ url ++ ", does it exist?")
      x:_ -> pure x

  let deps = cursor
         $// element "a"
         >=> attributeIs "class" "deplink__link"
         &// content
  pure (Package (T.unpack repo) (map T.unpack deps))

hasContent :: Text -> Axis
hasContent t c
  | T.strip (T.concat (c $// content)) == t = [c]
  | otherwise = []

-- | Get the commit SHA for the master branch
--
-- Technically will take whatever is the displayed branch on the
-- Github UI
getMasterCommit :: String -- ^ repo URL
                -> IO String
getMasterCommit repo = do
  req <- parseRequest repo
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
  case oldStyle ++ newStyle of
    [x] -> pure (T.unpack (T.reverse (T.takeWhile (/= '/') (T.reverse x))))
    _   -> error ("Could not find commit from " ++ repo)
