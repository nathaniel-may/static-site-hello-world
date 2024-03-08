#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Options.Applicative
import System.Exit (ExitCode(..))
import Turtle

data Command = Build | Serve | Develop | Clean deriving (Read, Show, Eq)

main :: IO ExitCode
main = do
    cmd <- customExecParser (prefs showHelpOnEmpty) (info
        (parser <**> helper)
        (fullDesc <> progDesc "Tools for developing and deploying your application"))
    run cmd

run :: MonadIO m => Command -> m ExitCode
run = liftIO . \case
    Build -> do
        proc "rm" ["-rf", "dist"] empty
            -- (.&&.) proc "cp" ["src/{index.html,live.js}", "dist/"] 
            -- (.&&.) proc "cp" ["assets/images", "dist/"]
            -- (.&&.) cp "assets/favicon/*" "dist/"
            -- todo webp image conversion here with `map`
        proc "spago" ["build"] empty
    -- "rm -rf dist && mkdir dist && cp src/{index.html,live.js} dist/ && mkdir dist/assets/ && cp -r assets/images dist/assets && cp assets/favicon/* dist/ && for file in dist/assets/images/*; do cwebp -quiet -q 80 \"$file\" -o \"${file%.*}.webp\"; done & npx tailwindcss -c tailwind.config.js -i ./src/style.css -o ./dist/style.css & spago bundle-app --to dist/index.js",

    Serve -> ExitSuccess <$ print "todo implement serve"
    Develop -> ExitSuccess <$ print "todo implement develop"
    Clean -> ExitSuccess <$ print "todo implement clean"

parser :: Parser Command
parser = subparser
     ( command "build"  (info (pure Build) ( progDesc "build everything" ))
    <> command "serve"  (info (pure Serve) ( progDesc "serve the web app" ))
    <> command "serve"  (info (pure Serve) ( progDesc "serve the web app" ))
    <> command "serve"  (info (pure Serve) ( progDesc "serve the web app" )) )
