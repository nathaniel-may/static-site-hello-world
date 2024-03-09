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
import Data.Maybe (fromMaybe, mapMaybe)
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
                -- root is the project root before build begins. files get copied where they belong from their tidy starting places
                -- dist is the directory the server serves from
                procs "rm" [ "-rf", "root", "dist" ] empty
                procs "mkdir" [ "-p", "root", "dist/images"] empty
                -- get every file in the project root so it can be moved to its pre-build location
                paths <- map T.pack <$> listShell (ls ".")
                -- copy whole dirs with their existing structure before moving individual files
                let dirMoves = [ ("./src", "./root/"), ("./test", "./root/") ]
                let fileMoves = mapMaybe (\path -> (path, ) <$> if
                        -- skip generated files
                        | any (`T.isPrefixOf` path) builtPathsText -> Nothing
                        -- if we have a destination for the whole directory, don't move the individual files
                        | any ((`T.isPrefixOf` path) . fst) dirMoves -> Nothing
                        -- these files don't move
                        | path `elem`
                            [ "./scripts.hs"
                            , "./README.md"
                            , "./LICENSE"
                            , "./diagram.png"
                            , "./.purs-repl"
                            , "./.psc-ide-port"
                            ] -> Nothing
                        -- these files move
                        | T.isPrefixOf "./assets/images" path -> Just "dist/images"
                        | T.isPrefixOf "./assets/favicon" path -> Just "dist/"
                        | T.isPrefixOf "./config" path -> Just "root/"
                        -- if the path hasn't been explicitly mapped, raise an error and don't build so every file is accounted for
                        | otherwise ->
                            error $ "unknown source file: " <> T.unpack path
                                <> "\nPlease designate its pre-build destination."
                        )
                        paths
                let boop = (fileMoves :: [(Text, Text)])
                traverse_ (\(src, dest) -> procs "cp" [ "-r", src, dest ] empty) dirMoves
                traverse_ (\(src, dest) -> procs "cp" [ src, dest ] empty) fileMoves
                -- todo generate index.html (it's not compiled so it should live somewhere special or be generated)

            -- bash: for file in dist/images/*; do cwebp -quiet -q 80 \"$file\" -o \"${file%.*}.webp\"; done
            buildImages = sh (do file <- ls "dist/images/"; liftIO (procs "cwebp" [ "-quiet", "-q", "80", T.pack file, "-o", T.pack $ replaceExtension file "webp" ] empty ))
            buildStyles = procs "npx" [ "tailwindcss"
                , "-c", "tailwind.config.js"
                , "-i", "./root/src/style.css"
                , "-o", "./dist/style.css" ] empty
            -- calling npm run instead of spago bundle-app directly so spago can find `esbuild` in local node_modules
            buildJS = sh (pushd "./root" >> inprocWithErr "npm" [ "run", "spago-bundle-app" ] empty)
            in moveStuff >> run (Run Install) >> mapConcurrently_ id [ buildStyles, buildJS, buildImages ]

        Serve   -> print "todo implement serve"
            -- look at pushd for serving in a different directory

        Develop -> print "todo implement develop"

        Install -> sh (pushd "./root" >> inprocWithErr "npm" [ "install" ] empty)

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

-- leading "./" is necessary to match out put of find
builtPaths :: [FilePath]
builtPaths =
    [ "./dist/"
    , "./node_modules/"
    , "./output/"
    , "./.spago/"
    , "./.stack-work/"
    , "./root/"
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

listShell :: Shell a -> IO [a]
listShell = flip foldShell (FoldShell (pure . pure) [] pure)

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

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
