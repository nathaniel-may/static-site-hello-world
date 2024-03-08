#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (when)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Options.Applicative
import System.Exit (ExitCode(..))
import Turtle

data Command = Build | Serve | Develop | Clean deriving (Read, Show, Eq)

main :: IO ()
main = do
    cmd <- customExecParser (prefs showHelpOnEmpty) parser
    run cmd

-- I use `proc "mkdir" ..` instead of `mkdir` etc. becuase turtle's commands pass over failures by returning () instead of ExitCode
run :: MonadIO m => Command -> m ()
run = liftIO . \case
    Build -> do
        let npmInstall = procs "npm" [ "install" ] empty
        let moveStuff = do
                procs "rm" [ "-rf", "dist" ] empty
                procs "mkdir" [ "dist/"] empty
                -- todo generate index.html (it's not compiled so it should live somewhere speical or be generated)
                procs "cp" [ "src/index.html", "dist/" ] empty
                -- this is a special case of uncompiled code. it should go in a directory of raw js somewhere
                procs "cp" [ "src/live.js", "dist/" ] empty
                procs "cp" [ "-r", "assets/images", "dist/" ] empty
                -- todo: the * doesn't resolve properly
                -- procs "cp" [ "assets/favicon/*", "dist/" ] empty
                -- todo: for file in dist/assets/images/*; do cwebp -quiet -q 80 \"$file\" -o \"${file%.*}.webp\"; done
                -- use this pattern: pages <- forConcurrently ["url1", "url2", "url3"] $ \url -> getURL url
        let buildStyles = procs "npx" [ "tailwindcss"
                , "-c", "tailwind.config.js"
                , "-i", "./src/style.css"
                , "-o", "./dist/style.css" ] empty
        let buildJS = procs "spago" [ "bundle-app", "--to", "dist/index.js" ] empty
        mapConcurrently_ id [ npmInstall, moveStuff, buildStyles, buildJS ]

    Serve   -> print "todo implement serve"
        -- look at pushd for serving in a different directory
    Develop -> print "todo implement develop"
    Clean   -> print "todo implement clean"

parser :: ParserInfo Command
parser = info
    (subcommands <**> helper)
    (fullDesc <> progDesc "Tools for developing and deploying your application")
    where
    subcommands :: Parser Command
    subcommands = subparser
        (  command "build"   (info (pure Build)   ( progDesc "build everything" ))
        <> command "serve"   (info (pure Serve)   ( progDesc "serve the web app" ))
        <> command "develop" (info (pure Develop) ( progDesc "serve and watch for source file changes to reload the page" ))
        <> command "clean"   (info (pure Clean)   ( progDesc "delete all build files" )) )

-- test :: IO ExitCode
-- test = do
--     let tests =
--             [ do
--                 actual <- execParserPure "build" parser
--                 when (Build /= actual) $ throw "bad build command parse"
--             , do
--                 actual <- execParserPure "serve" parser
--                 when (Build /= actual) $ throw "bad serve command parse"
--             ]

--     mapConcurrently_ tests