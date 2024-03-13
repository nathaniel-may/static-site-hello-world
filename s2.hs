#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay, killThread, forkOS)
import Control.Concurrent.Async (Async, uninterruptibleCancel, cancel, forConcurrently_, mapConcurrently_, withAsync)
import Control.Exception (Exception(..), throwIO)
import Control.Monad (when)
import Control.Monad.Managed (MonadManaged)
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
data Command = Build | BuildRelease | Serve | Develop | Install | Clean deriving (Read, Show, Eq)

newtype ScriptException = ScriptException ByteString deriving (Show)
instance Exception ScriptException

newtype TestingException = TestingException Text deriving (Show)
instance Exception TestingException

main :: IO ()
main = do
    server <- forkOS . sh $ pushd "dist" *> procs "npx" [ "http-server",  "-c-1", "--port", "8111" ] empty
    let line = inproc 
            "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" 
            [ "--headless=new", "--dump-dom", "http://127.0.0.1:8111" ]
            empty
    sh (output "index2.html" line)
    killThread server
        

-- THIS WORKS
-- main :: IO ()
-- main = sh $ do
--     pushd "dist"
--     liftIO $ withAsync 
--         (sh $ procs "npx" [ "http-server",  "-c-1", "--port", "8111" ] empty)
--         -- give the server 0.1s head start to load everything TODO is this necessary?
--         (\_ -> do
--             -- use headless chrome to generate the html and append each line of the html to the dist file
--             -- todo minify it?
--             let line = inproc 
--                     -- todo this path is mac specific
--                     "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" 
--                     [ "--headless=new", "--dump-dom", "http://127.0.0.1:8111" ]
--                     empty
--             sh (output "index2.html" line)
--             -- kill the server via the OS since it spawned it's own thread outside the haskell runtime
--             sh $ procs "killall" ["http-server"] empty
--         )

-- DOESN'T HANG
-- main :: IO ()
-- main = sh $ do
--     liftIO $ withAsync 
--         -- emulating server without server
--         (traverse (\i -> output ("boop"<> show i <> ".txt") (pure mempty) <* threadDelay 200000 ) [0..20])
--         -- give the server 0.1s head start to load everything TODO is this necessary?
--         (\server -> do
--             sh (output "index2.html" (pure $ unsafeTextToLine "minified example output"))
--             -- kill the server once all the lines have been dumped
--             uninterruptibleCancel server
--         )

-- main :: IO ()
-- main = sh $ do
--     liftIO $ withAsync 
--         (sh $ procs "npx" [ "http-server",  "-c-1", "--port", "8111" ] empty)
--         -- give the server 0.1s head start to load everything TODO is this necessary?
--         (\server -> do
--             -- use headless chrome to generate the html and append each line of the html to the dist file
--             -- todo minify it?
--             let line = inproc 
--                     -- todo this path is mac specific
--                     "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" 
--                     [ "--headless=new", "--dump-dom", "http://127.0.0.1:8111" ]
--                     empty
--             sh (output "index2.html" line)
--             -- kill the server once all the lines have been dumped
--             uninterruptibleCancel server
--         )

-- main :: IO ()
-- main = sh $ do
--     pushd "dist"
--     liftIO $ withAsync 
--         (sh $ procs "npx" [ "http-server",  "-c-1", "--port", "8111" ] empty)
--         -- give the server 0.1s head start to load everything TODO is this necessary?
--         (\server -> do
--             -- use headless chrome to generate the html and append each line of the html to the dist file
--             -- todo minify it?
--             let line = inproc 
--                     -- todo this path is mac specific
--                     "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" 
--                     [ "--headless=new", "--dump-dom", "http://127.0.0.1:8111" ]
--                     empty
--             sh (output "index2.html" line)
--             -- kill the server once all the lines have been dumped
--             uninterruptibleCancel server
--         )
