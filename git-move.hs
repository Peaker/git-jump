{-# LANGUAGE NoImplicitPrelude #-}
import           System.Process
import qualified Text.Parsec as P
import           Prelude.Compat

type Parser = P.Parsec String ()

nulTerminatedStr :: Parser String
nulTerminatedStr = P.many (P.satisfy (/= toEnum 0)) <* P.char (toEnum 0)

porcelainEntry :: Parser [FilePath]
porcelainEntry =
    do
        indexChar <- P.anyChar
        _workingTreeChar <- P.anyChar
        _ <- P.char ' '
        case indexChar of
            'R' ->
                do
                    destFilePath <- nulTerminatedStr
                    srcFilePath <- nulTerminatedStr
                    return [destFilePath, srcFilePath]
            _ -> (:[]) <$> nulTerminatedStr

porcelain :: Parser [FilePath]
porcelain = concat <$> P.many porcelainEntry

getUnclean :: IO [FilePath]
getUnclean =
    do
        stdout <- readProcess "git" ["status", "--porcelain", "-z"] ""
        return $ either (fail . show) id $ P.runParser porcelain () "" stdout

main :: IO ()
main =
    do
        uncleanPaths <- getUnclean
        print uncleanPaths
