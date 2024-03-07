module Main where

import qualified Data.Char as Char
import Options.Applicative
import Text.Read (readMaybe)
import Turtle

data Command = Build | Serve | Develop | Clean deriving (Read, Show, Eq)

main :: IO ()
main = do
    cmd <- customExecParser (prefs showHelpOnEmpty) (info
        (parser <**> helper)
        (fullDesc <> progDesc "Tools for developing and deploying your application"))
    print $ Build == cmd
    -- view $ ls "."

run :: MonadIO m => Command -> m ()
run Build = liftIO $ print "todo implement build"
run Serve = liftIO $ print "todo implement serve"
run Develop = liftIO $ print "todo implement develop"
run Clean = liftIO $ print "todo implement clean"

parser :: Parser Command
parser = subparser
     ( command "build"  (info empty ( progDesc "boop" ))
    <> command "serve"  (info empty ( progDesc "beep" )) )
