
module Main(main) where

import Control.Exception
import Control.Monad
import Data.Char
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath


main = do
    let obj = "obj"
    eps <- files "graphics" "eps"
    tex <- files "." "tex"
    bib <- files "." "bib"

    for eps $
        \e -> replaceDirectory e obj <== [e] $ copyFile
    for bib $
        \b -> replaceDirectory b obj <== [b] $ \from to -> do
            copyFile from to
            system_ "obj" $ "bibtex -quiet " ++ takeBaseName (takeFileName b)
    for tex $
        \t -> replaceDirectory t obj <== [t,"thesis.fmt"] $ \from to -> do
            system_ "." $ "lhs2tex " ++ from ++ " -o " ++ to

    system_ "obj" "texify --quiet thesis.tex"
    copyFile "obj/thesis.dvi" "thesis.dvi"


for = flip mapM

files dir ext = do
    s <- getDirectoryContents dir
    s <- return $ filter ((==) ('.':ext) . takeExtension) s
    return $ map (dir </>) s


system_ dir cmd = do
    orig <- getCurrentDirectory
    bracket_ (setCurrentDirectory dir) (setCurrentDirectory orig) $ do
        putStrLn cmd
        res <- system $ cmd ++ " > stdout.txt 2> stderr.txt"
        out <- readFile "stdout.txt"
        err <- readFile "stderr.txt"
        putStr $ out ++ err
        when (res /= ExitSuccess) $ do
            reportError out
            putStrLn "System command failed! Press enter to continue"
            getChar
            exitWith (ExitFailure 1)


reportError :: String -> IO ()
reportError s2 = do
        b <- doesFileExist file
        when (b && not (null pos) && all isDigit pos) $ do
            src <- readFile file
            putStr $ unlines $ map f $ take 7 $ drop (read pos - 3) $ zip [1..] $ lines src
    where
        s = last $ "" : lines s2
        (pre,post) = splitAt 3 s
        (front,rest) = break (== ':') post
        file = pre ++ front
        pos = takeWhile (/= ':') $ drop 1 rest
        f (p,s) = show p ++ " : " ++ s


(<==) :: FilePath -> [FilePath] -> (FilePath -> FilePath -> IO ()) -> IO ()
(<==) to froms@(from:_) action = do
    b <- doesFileExist to
    rebuild <- if not b then return True else do
        from2 <- liftM maximum $ mapM getModificationTime froms
        to2 <- getModificationTime to
        return $ to2 < from2
    when rebuild $ do
        putStrLn $ "Building: " ++ to
        action from to
