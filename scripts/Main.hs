{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative
import Turtle

data Command = Build | Serve | Develop | Clean deriving (Read, Show, Eq)

main :: IO ()
main = do
    cmd <- customExecParser (prefs showHelpOnEmpty) (info
        (parser <**> helper)
        (fullDesc <> progDesc "Tools for developing and deploying your application"))
    run cmd

run :: MonadIO m => Command -> m ()
run = liftIO . \case
    Build -> print "todo implement build"
    Serve -> print "todo implement serve"
    Develop -> print "todo implement develop"
    Clean -> print "todo implement clean"

parser :: Parser Command
parser = subparser
     ( command "build"  (info (pure Build) ( progDesc "build everything" ))
    <> command "serve"  (info (pure Serve) ( progDesc "serve the web app" ))
    <> command "serve"  (info (pure Serve) ( progDesc "serve the web app" ))
    <> command "serve"  (info (pure Serve) ( progDesc "serve the web app" )) )
