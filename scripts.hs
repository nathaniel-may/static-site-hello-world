#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE LambdaCase, OverloadedStrings, MultiWayIf #-}

module Main where

import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Exception (Exception(..), throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import System.Console.ANSI
import System.Exit (ExitCode(..))
import System.FilePath (replaceExtension)
import qualified System.IO as SYS
import Turtle

data App = Run Command | Test deriving (Read, Show, Eq)
data Command = Build | Serve | Develop | Install | Clean deriving (Read, Show, Eq)

newtype ScriptException = ScriptException ByteString deriving (Show)
instance Exception ScriptException

newtype TestingException = TestingException Text deriving (Show)
instance Exception TestingException

main :: IO ()
main = do
    app <- customExecParser (prefs showHelpOnEmpty) parser
    run app

-- I use `proc "mkdir" ..` instead of `mkdir` etc. becuase turtle's commands pass over failures by returning () instead of ExitCode
run :: MonadIO m => App -> m ()
run = liftIO . \case
    -- todo: optionally run the tests before every command execution
    -- Run cmd -> run Test *> case cmd of
    Run cmd -> case cmd of
        Build -> let
            moveStuff = do
                -- dist is the directory the server serves from. wipe the previous build output and start over
                procs "rm" [ "-rf", "dist" ] empty
                mkdir "dist"
                -- todo generate index.html (it's not compiled so it should live somewhere special or be generated)
                -- copy whole dirs with their existing structure before moving individual files
                procs "cp" [ "-r", "./assets/images", "dist" ] empty
                -- copy all the favicons to the root of the server folder
                ls "./assets/favicon" >>= (`cp` "dist")
            -- bash: for file in dist/images/*; do cwebp -quiet -q 80 \"$file\" -o \"${file%.*}.webp\"; done
            buildImages = do file <- ls "dist/images/"; procs "cwebp" [ "-quiet", "-q", "80", T.pack file, "-o", T.pack $ replaceExtension file "webp" ] empty
            buildStyles = procs "npx" [ "tailwindcss"
                , "-c", "tailwind.config.js"
                , "-i", "./src/style.css"
                , "-o", "./dist/style.css" ] empty
            -- calling npm run instead of spago bundle-app directly so spago can find `esbuild` in local node_modules
            buildJS = procs "npm" [ "run", "spago-bundle-app" ] empty
            in view $ do view moveStuff; run (Run Install); liftIO $ mapConcurrently_ view [ buildStyles, buildJS, buildImages ]

        Serve   -> print "todo implement serve"
            -- look at pushd for serving in a different directory

        Develop -> print "todo implement develop"

        Install -> view $ procs "npm" [ "install" ] empty

        -- todo overkill to concurrently do this
        Clean -> forConcurrently_ builtPaths (\dir -> procs "rm" [ "-rf", T.pack dir ] empty )

    Test -> let
        tests =
            [ test "build command parses"
                (handleParseResult $ execParserPure (prefs showHelpOnEmpty) parser [ "build" ] )
                (== Run Build)
                "expected build input to produce Build result"
            ]
        in do
            mapConcurrently_ id tests

-- leading "./" and lack of trailing "/" is necessary to match out put of find
builtPaths :: [FilePath]
builtPaths =
    [ "./dist"
    , "./node_modules"
    , "./output"
    , "./.spago"
    , "./.stack-work"
    ]

builtPathsText :: [Text]
builtPathsText = map T.pack builtPaths

parser :: ParserInfo App
parser = info
    (subcommands <**> helper)
    (fullDesc <> progDesc "Tools for developing and deploying your application")
    where
    subcommands :: Parser App
    subcommands = subparser
        (  command "build"        (info (pure $ Run Build)   ( progDesc "build everything" ))
        <> command "serve"        (info (pure $ Run Serve)   ( progDesc "serve the web app" ))
        <> command "develop"      (info (pure $ Run Develop) ( progDesc "serve and watch for source file changes to reload the page" ))
        <> command "install"      (info (pure $ Run Install) ( progDesc "install build dependencies" ))
        <> command "clean"        (info (pure $ Run Clean)   ( progDesc "delete all build files" ))
        <> command "test-scripts" (info (pure Test)          ( progDesc "test all the scripts in this file" )) )

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

for_  :: Functor f => f a -> (a -> b) -> f ()
for_ x f = void $ fmap f x

test :: MonadIO m => Text -> m a -> (a -> Bool) -> Text -> m ()
test name actual fExpected msg = do
    result <- fExpected <$> actual
    let errOutput = do
            T.putStrLn "‼️ TEST FAILED ‼️"
            throwIO (TestingException msg)
    if result then pure () else liftIO $ do
        stdoutSupportsANSI <- hNowSupportsANSI SYS.stdout
        if stdoutSupportsANSI
        then do
            setSGR  [ SetConsoleIntensity BoldIntensity
                    , SetColor Foreground Vivid Red
                    ]
            errOutput
            setSGR [Reset]
        else
            errOutput
