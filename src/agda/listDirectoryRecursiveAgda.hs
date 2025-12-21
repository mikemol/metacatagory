-- listDirectoryRecursiveAgda.hs
-- Haskell helper for Agda FFI: recursively list all .agda files under a given root directory

module Main where

import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>, takeExtension))
import Control.Monad (forM, filterM)
import System.Environment (getArgs)

listAgdaFiles :: FilePath -> IO [FilePath]
listAgdaFiles root = do
    isDir <- doesDirectoryExist root
    if not isDir then return [] else do
        contents <- listDirectory root
        let paths = map (root </>) contents
        files <- filterM (fmap not . doesDirectoryExist) paths
        dirs  <- filterM doesDirectoryExist paths
        agdaFiles <- return $ filter ((== ".agda") . takeExtension) files
        nested <- fmap concat $ forM dirs listAgdaFiles
        return (agdaFiles ++ nested)

main :: IO ()
main = do
    args <- getArgs
    let root = if null args then "." else head args
    files <- listAgdaFiles root
    mapM_ putStrLn files
