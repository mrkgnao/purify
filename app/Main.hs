{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative
import           Control.Exception.Safe
import           Data.Aeson
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Map                                  (Map)
import qualified Data.Map                                  as Map
import           Data.Maybe
import           Data.Yaml
import           Options.Applicative.Simple

import           System.Directory
import           System.Environment
import           System.Exit                               hiding (die)
import qualified System.Exit
import qualified System.FilePath.Glob                      as Glob
import qualified System.FSNotify                           as FS
import           System.Process

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict

import           PackageInfo

import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import qualified Data.Text.IO                              as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Data.Monoid                               (mconcat)
import           Debug.Trace

die = System.Exit.die . T.unpack

data Dep = Dep
  { depRepo    :: Text
  , depCommit  :: Text
  , depName    :: Text
  , depModules :: Maybe [Text]
  , depDeps    :: [Text]
  } deriving (Eq,Show)

instance ToJSON Dep where
  toJSON Dep {..} =
    object $
    maybe
      id
      (\m -> (("modules" .= m) :))
      depModules
      [ "repo" .= depRepo
      , "commit" .= depCommit
      , "name" .= depName
      , "deps" .= depDeps
      ]

instance FromJSON Dep where
  parseJSON j = do
    o <- parseJSON j
    repo <- o .: "repo"
    commit <- o .: "commit"
    let name' =
          T.takeWhile
            (/= '.')
            (T.reverse (T.takeWhile (/= '/') (T.reverse repo)))
    name <- o .:? "name" .!= name'
    mmodules <- o .:? "modules"
    deps <- o .:? "deps" .!= []
    pure (Dep repo commit name mmodules deps)

data Purify = Purify
  { outputFile :: FilePath
  , extraDeps  :: [Dep]
  } deriving (Eq,Show)

instance ToJSON Purify where
  toJSON (Purify outFile deps) = object
    [ "output-file" .= outFile
    , "extra-deps" .= deps
    ]

instance FromJSON Purify where
  parseJSON j = do
    o <- parseJSON j
    outputFile <- o .: "output-file"
    extraDeps <-
      ((o .: "extra-deps") <|>
       fmap flattenDeps (o .: "extra-deps")) {-backwards compat-}
    pure (Purify {..})
    where
      flattenDeps :: Map Text Dep -> [Dep]
      flattenDeps = map (\(k, v) -> v { depName = k }) . Map.toList

main :: IO ()
main = do
  exists <- doesFileExist "purify.yaml"
  if not exists
    then die "Expected purify.yaml in the directory of your PureScript project."
    else do
      result <- decodeFileEither "purify.yaml"
      case result of
        Left _ -> die "Couldn't parse purify.yaml file."
        Right config -> do
          args <- getArgs
          if null args
            then purify [] config False False
            else join $ fmap snd $ simpleOptions
                 "VERSION"
              "purify build tool for PureScript"
              "Fully reproducible builds for PureScript"
              (pure ()) $ do
              addCommand "build" "Build code" id $ purify
                  <$> pure []
                  <*> pure config
                  <*> switch (long "file-watch" <> help "Auto-rebuild on file change")
                  <*> switch (long "verbose" <> help "Show git log")
              addCommand "ide" "Launch IDE interaction" id $ pure ide
              addCommand "add-deps" "Add dependencies to purify.yaml" id $ addDeps
                  <$> pure config
                  <*> fmap (map T.pack) (some (strArgument (metavar "PACKAGE-NAME")))
                  <*> switch (long "implicit-prefix" <> help "Add the purescript- prefix automatically")

data FetchState = Pending | Fetched

purify :: [FilePath] -> Purify -> Bool -> Bool ->  IO ()
purify inputFiles config fileWatch verbose = do
  let quietness =
        if verbose
          then []
          else ["-q"]
  createDirectoryIfMissing True ".purify-work/extra-deps"
  let extraDepMismatch = nub (extraDeps config) /= extraDeps config
      extraDepNameMismatch =
        nubBy (on (==) depName) (extraDeps config) /= extraDeps config
  when
    (extraDepMismatch || extraDepNameMismatch)
    (die "Dependencies contain duplicates.")
  forM_ (extraDeps config) $ \dep -> do
    let depDir = T.pack depDir'
        depDir' = getDepDir dep
        gitDir = depDir
    exists <- doesDirectoryExist depDir'
    let clone =
          if exists
            then checkout
            else do
              logPhase ("Cloning " <> depName dep <> " ...")
              ok <-
                rawSystemLog
                  "git"
                  (["clone"] <> quietness <> [depRepo dep, depDir])
              case ok of
                ExitFailure {} ->
                  die
                    ("Failed to clone package " <> depName dep <> " from " <>
                     depRepo dep)
                _ -> checkout
        checkout = do
          tags <-
            fmap
              (map T.pack . lines)
              (readProcess
                 "git"
                 ["-C", T.unpack gitDir, "tag", "--points-at", "HEAD"]
                 "")
          unless (depCommit dep `elem` tags) $ do
            cur <-
              fmap T.pack $
              readProcess "git" ["-C", T.unpack gitDir, "rev-parse", "HEAD"] ""
            let commit = T.takeWhile (const True) cur
                shortDepCommit = T.take 7 (depCommit dep)
            unless (commit == depCommit dep) (fetch shortDepCommit Pending)
        fetch shortDepCommit fetchState = do
          case fetchState of
            Pending ->
              T.putStrLn
                ("Checking out " <> depName dep <> " (" <> shortDepCommit <>
                 ") ...")
            _ -> pure ()
          res <-
            rawSystemLog
              "git"
              (["-C", gitDir, "checkout", "-f"] <> quietness <> [depCommit dep])
          whenFailure res $
            case fetchState of
              Pending -> do
                putStrLn "Failed to checkout, fetching latest from remote ..."
                fres <-
                  rawSystemLog "git" (["-C", gitDir, "fetch"] <> quietness)
                withErrorMsg
                  ("Tried to fetch " <> depCommit dep <>
                   " from the remote, but that failed too. Giving up.")
                  fres $
                  fetch shortDepCommit Fetched
              Fetched ->
                die
                  ("Checking out version failed for " <> depName dep <> ": " <>
                   depCommit dep)
    clone
  srcExists <- doesDirectoryExist "src/"
  if not srcExists
    then die
           "There is no src/ directory in this project. Please create one and put your PureScript files in there."
    else let dirs =
               map
                 (<> "/src")
                 ("." :
                  map
                    getDepDir
                    (filter (isNothing . depModules) (extraDeps config)))
             buildCmd = purifyDirs inputFiles config dirs
         in if fileWatch
              then watchDirs dirs buildCmd
              else buildCmd

rawSystemLog :: Text -> [Text] -> IO ExitCode
rawSystemLog s args = do
  let bound = 200
      args' =
        if T.length (T.unwords args) < bound
          then T.unwords args
          else let beg = take 3 args
                   end = take 1 (reverse args)
               in T.unwords beg <> " ... " <> T.unwords end
  -- print (length (unwords args))
  -- print args
  T.putStrLn ("Running " <> s <> " " <> args')
  rawSystem (T.unpack s) (map T.unpack args)

ignore (FS.Added ('.' : _) _)    = True
ignore (FS.Modified ('.' : _) _) = True
ignore (FS.Removed ('.' : _) _)  = True
ignore _                         = False

watchDirs :: [FilePath] -> IO () -> IO ()
watchDirs dirs inner = do
  toRunVar <- newTVarIO True -- do an initial build immediately
  FS.withManager
    (\manager -> do
       forM_ dirs $ \dir ->
         FS.watchTree
           manager
           dir
           (\x -> trace (show x) (not (ignore x)))
           (const (atomically (writeTVar toRunVar True)))
       forever
         (do atomically
               (do toRun <- readTVar toRunVar
                   check toRun
                   writeTVar toRunVar False)
             putStrLn "Starting build"
             eres <- tryAny inner
             case eres of
               Left e   -> print e
               Right () -> pure ()
             putStrLn "Build command finished, waiting for file changes\n"))

getDepDir :: Dep -> FilePath
getDepDir dep = T.unpack $ ".purify-work/extra-deps/" <> depName dep

tshow = T.pack . show

purifyDirs :: [FilePath]
           -> Purify
           -> [FilePath]
           -> IO ()
purifyDirs inputFiles config dirs = do
  let pat = Glob.compile "**/*.purs"
  foundPurs <- concat <$> mapM (Glob.globDir1 pat) dirs
  let explicitPurs =
        mconcat
          (mapMaybe
             (\dep -> do
                modules <- depModules dep
                pure
                  (map
                     (\modn -> getDepDir dep <> "/" <> topath modn)
                     (fmap T.unpack modules)))
             (extraDeps config))
        where
          topath m = "src/" <> replace m <> ".purs"
          replace ('.':cs) = '/' : replace cs
          replace (c:cs)   = c : replace cs
          replace []       = []
  let allPurs = fmap T.pack $ inputFiles <> foundPurs <> explicitPurs
  logPhase ("Compiling " <> tshow (length allPurs) <> " modules ...")
  let outputDir = ".purify-work/js-output"
  status <- rawSystemLog "purs" (["compile", "-o", outputDir] <> allPurs)
  withErrorMsg "Compile failed." status $ do
    logPhase "Bundling ..."
    stat <-
      rawSystemLog
        "purs"
        [ "bundle"
        , ".purify-work/js-output/**/*.js"
        , "-m"
        , "Main"
        , "--main"
        , "Main"
        , "-o"
        , T.pack $ outputFile config
        ]
    withErrorMsg "Bundling failed." stat $
      putStrLn ("Output bundled to " <> outputFile config)

withErrorMsg :: Text -> ExitCode -> IO a -> IO a
withErrorMsg msg (ExitFailure _) _ = die msg
withErrorMsg _ _ act               = act

whenFailure :: ExitCode -> IO () -> IO ()
whenFailure (ExitFailure _) act = act
whenFailure _ _                 = pure ()

ide :: IO ()
ide =
  rawSystemLog
    "purs"
    [ "ide"
    , "server"
    , "--output-directory"
    , ".purify-work/js-output"
    , ".purify-work/extra-deps/*/src/**/*.purs"
    , "src/**/*.purs"
    ] >>=
  exitWith

addDeps :: Purify -> [Text] -> Bool -> IO ()
addDeps (Purify outFile deps) newDeps autoprefix =
  void (runStateT (mapM_ (addDep outFile []) newDeps') depsMap)
  where
    depsMap = Map.unions (map (\dep -> Map.singleton (depName dep) dep) deps)
    newDeps' =
      if autoprefix
        then map ("purescript-" <>) newDeps
        else newDeps

addDep :: FilePath -- ^ out file
       -> [Text] -- ^ call stack, to avoid cycles
       -> Text   -- ^ new dep
       -> StateT (Map Text Dep) IO ()
addDep _ depStack newDep
  | newDep `elem` depStack = error ("Dep cycle detected: " <> show (newDep : depStack))
addDep outFile depStack newDep = do
  let newStack = newDep : depStack
  allDeps <- get
  case Map.lookup newDep allDeps of
    Nothing -> do
      liftIO (T.putStrLn ("Adding dep: " <> newDep))
      (Package repo deps) <- liftIO (lookupPackage newDep)
      master <- liftIO (getMasterCommit repo)
      modify (Map.insert newDep (Dep
        { depRepo = repo
        , depCommit = master
        , depName = newDep
        , depModules = Nothing
        , depDeps = deps
        }))
      deps' <- get
      liftIO (encodeFile "purify.yaml" (Purify outFile (Map.elems deps')))
      mapM_ (addDep outFile newStack) deps
    Just ed -> mapM_ (addDep outFile newStack) (depDeps ed)

logPhase :: Text -> IO ()
logPhase = T.putStrLn . renderStrict . layoutPretty defaultLayoutOptions . color Red . pretty

