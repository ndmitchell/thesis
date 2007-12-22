
module Main(main) where

import System.Directory
import System.FilePath
import System.Cmd
import System.Exit
import Control.Exception
import Control.Monad


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
            system_ "obj" $ "bibtex " ++ takeFileName b
    for tex $
        \t -> replaceDirectory t obj <== [t,"thesis.fmt"] $ \from to -> do
            system_ "." $ "lhs2tex " ++ from ++ " -o " ++ to

    system_ "obj" "texify thesis.tex"
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
        res <- system cmd
        when (res /= ExitSuccess) $ error "System command failed!"

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
