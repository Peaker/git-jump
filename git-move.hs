{-# LANGUAGE NoImplicitPrelude #-}
import           Data.List (isSuffixOf)
import           System.Environment (getArgs)
import           System.Process
import qualified System.IO as IO

import           Prelude.Compat

chomp :: String -> String
chomp str
    | "\n" `isSuffixOf` str = init str
    | otherwise = str

git :: [String] -> IO String
git args = readProcess "git" args ""

tryGit :: [String] -> IO (Either Int String)
tryGit args =
    do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "git" args ""
        IO.hPutStr IO.stderr stderr
        case exitCode of
            ExitFailure i ->
                do
                    putStr stdout
                    return $ Left i
            ExitSuccess -> return $ Right stdout

main :: IO ()
main =
    do
        [destRef] <- getArgs
        origPos <- chomp <$> git ["rev-parse", "HEAD"]
        _ <- git ["stash", "-u"]
        _ <- git ["diff", "--quiet"] -- sanity check that there's no diff
        _ <- git ["reset", "--hard", destRef]
        res <- tryGit ["stash", "pop"]
        case res of
            Left _errCode ->
                do
                    putStrLn $ "Cannot apply working tree changes (via stash), returning to: " ++ show origPos
                    git ["reset", "--hard", origPos]
                    git ["stash", "pop"]
            Right -> return ()
