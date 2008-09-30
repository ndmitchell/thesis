
module Main where

import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.FilePath


boring = ["import","hiding","(",")",",","module","->","[","]"
         ,"forall","<-","infix","infixr","deriving","=","|"
         ,"data","`","::","if","then","else","where","}"
         ,"{","let","in","case","of","...","..","do"]
         ++
         ["alpha","beta"]


main = do
    index <- readIndex "thesis.ind"
    files <- getDirectoryContents "."
    files <- return $ filter ((==) ".tex" . takeExtension) files
    mapM_ (checkTex index) files


readIndex file = do
    src <- readFile file
    return [takeWhile (/= ',') $ drop 8 x | x <- lines src, "  \\item " `isPrefixOf` x]


readTex file = do
    src <- readFile file
    return $ concatMap f $ tails src
    where
        f xs | "\\begin{code}" `isPrefixOf` xs = readCode $ scanEnd $ drop 12 xs
             | otherwise = []


scanEnd xs | "\\end{code}" `isPrefixOf` xs = []
scanEnd (x:xs) = x : scanEnd xs


readCode xs = case lex $ dropWhile isSpace xs of
                  [("--",y)] -> readCode $ dropWhile (/= '\n') y
                  [("","")] -> []
                  [] -> []
                  [(x,y)] -> [x | good x] ++ readCode y
    where
        good (x:xs) = not $ x `elem` "'\"" || isDigit x


checkTex index file = do
    putStrLn $ "Checking index for " ++ file
    used <- readTex file
    let bad = nub used \\ (boring ++ index)
    when (bad /= []) $
        putStrLn $ "Bad index items in " ++ file ++ ": " ++ unwords bad
