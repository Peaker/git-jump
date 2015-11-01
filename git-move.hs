{-# LANGUAGE NoImplicitPrelude #-}
import           Control.Exception (onException)
import           Control.Monad (void)
import           Data.List (isSuffixOf)
import           System.Environment (getArgs, getProgName)
import           System.Process

import           Prelude.Compat

chomp :: String -> String
chomp str
    | "\n" `isSuffixOf` str = init str
    | otherwise = str

git :: String -> [String] -> IO String
git cmd args = readProcess "git" (cmd:args) ""

git_ :: String -> [String] -> IO ()
git_ cmd = void . git cmd

revParse :: [String] -> IO String
revParse args = chomp <$> git "rev-parse" args

reset :: [String] -> IO ()
reset = git_ "reset"

logErrors :: IO a -> String -> IO a
logErrors action msg = action `onException` putStrLn msg

main :: IO ()
main =
    do
        progName <- getProgName
        args <- getArgs
        destRef <-
            case args of
            [] -> revParse ["--symbolic-full-name", "@{u}"]
            [refSpec] -> return refSpec
            _ -> fail $ "Usage: " ++ progName ++ "[refspec]\n    if refspec is not provided, the remote tracked branch is used"
        origPos <- revParse ["HEAD"]
        git_ "commit" ["--allow-empty", "-mSTAGING"]
        staging <- revParse ["HEAD"]
        git_ "commit" ["--allow-empty", "-amUNSTAGED"]
        unstaged <- revParse ["HEAD"]
        let restore =
                do
                    reset ["--hard", unstaged]
                    reset ["--mixed", staging]
                    reset ["--soft", origPos]
                    return ()
        (`onException` restore) $
            do
                reset ["--hard", destRef]
                    `logErrors` ("Failed to move to " ++ show destRef)
                git_ "cherry-pick" ["--allow-empty", staging]
                git_ "cherry-pick" ["--allow-empty", unstaged]
                reset ["--mixed", "HEAD^"]
                reset ["--soft", "HEAD^"]
                return ()
