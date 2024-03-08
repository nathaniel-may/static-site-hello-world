{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Options.Applicative
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
    Build -> proc "spago" ["build"] empty

    Serve -> ExitSuccess <$ print "todo implement serve"
    Develop -> ExitSuccess <$ print "todo implement develop"
    Clean -> ExitSuccess <$ print "todo implement clean"

parser :: Parser Command
parser = subparser
     ( command "build"  (info (pure Build) ( progDesc "build everything" ))
    <> command "serve"  (info (pure Serve) ( progDesc "serve the web app" ))
    <> command "serve"  (info (pure Serve) ( progDesc "serve the web app" ))
    <> command "serve"  (info (pure Serve) ( progDesc "serve the web app" )) )
