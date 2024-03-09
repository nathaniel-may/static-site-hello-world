#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (forConcurrently_, mapConcurrently_)
import Control.Exception (Exception(..), throwIO)
import Control.Monad (when)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
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

newtype TestingException = TestingException Text deriving (Show)
instance Exception TestingException

main :: IO ()
main = do
    app <- customExecParser (prefs showHelpOnEmpty) parser
    run app

-- I use `proc "mkdir" ..` instead of `mkdir` etc. becuase turtle's commands pass over failures by returning () instead of ExitCode
run :: MonadIO m => App -> m ()
run = liftIO . \case
    Run Build -> let
        npmInstall = procs "npm" [ "install" ] empty
        moveStuff = do
            procs "rm" [ "-rf", "dist" ] empty
            procs "mkdir" [ "-p", "dist/images"] empty
            -- todo generate index.html (it's not compiled so it should live somewhere speical or be generated)
            procs "cp" [ "src/index.html", "dist/" ] empty
            -- this is a special case of uncompiled code. it should go in a directory of raw js somewhere
            procs "cp" [ "src/live.js", "dist/" ] empty
            procs "cp" [ "-r", "assets/images", "dist/" ] empty
            -- bash: cp assets/favicon/* dist/
            sh (do file <- ls "assets/favicon/"; liftIO (procs "cp" [ T.pack file, "dist/" ] empty ))
            -- bash: for file in dist/images/*; do cwebp -quiet -q 80 \"$file\" -o \"${file%.*}.webp\"; done
            -- TODO do concurrently?
            sh (do file <- ls "dist/images/"; liftIO (procs "cwebp" [ "-quiet", "-q", "80", T.pack file, "-o", T.pack $ replaceExtension file "webp" ] empty ))
        buildStyles = procs "npx" [ "tailwindcss"
            , "-c", "tailwind.config.js"
            , "-i", "./src/style.css"
            , "-o", "./dist/style.css" ] empty
        buildJS = procs "npm" [ "run", "spago-bundle-app" ] empty
        in mapConcurrently_ id [ npmInstall, moveStuff, buildStyles, buildJS ]

    Run Serve   -> print "todo implement serve"
        -- look at pushd for serving in a different directory

    Run Develop -> print "todo implement develop"

    Run Install -> print "todo implement install"
        -- npm i

    Run Clean -> let
        dirs =
            [ "dist/"
            , "node_modules/"
            , "output/"
            , ".spago/"
            , ".stack-work/"
            ]
        in forConcurrently_ dirs (\dir ->
            procs "rm" [ "-rf", dir ] empty
        )

    Test -> let
        tests =
            [ test "build command parses"
                (handleParseResult $ execParserPure (prefs showHelpOnEmpty) parser [ "build" ] )
                (== Run Build)
                "expected build input to produce Build result"
            ]
        in do
            mapConcurrently_ id tests

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
