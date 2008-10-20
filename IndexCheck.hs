
module Main where

import Control.Arrow
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
         ["alpha","beta"] ++ words extra

extra = unwords $
  ["While CoreData CoreExpr Core CoreFunc CoreVar CoreApp CoreCase CoreLet UniplateExpr uniplateExpr ns"
  ,"childrenExpr universeExpr descendExpr transformExpr plateListDiff res"
  ,"asTypeOf cast gmapQ gmapM isAlgType dtyp ctrs fromConstr dataTypeConstrs"
  ,"dataTypeOf uni_variables syb_variables everything mkQ com_variables"
  ,"uni_zeroCount syb_zeroCount com_zeroCount simp uni_simplify syb_simplify everywhere"
  ,"mkT com_simplify Stm SDecl Typ SAss Exp SBlock SReturn EStm EAdd EInt V T_int T_float"
  ,"ren uni_rename syb_rename com_rename uni_symbols syb_symbols com_symbols optimise"
  ,"uni_constFold syb_constFold com_constFold Manager Employee Address Company C Dept D PU DU E"
  ,"Person Salary P S incS uni_increase syb_increase com_increase uni_incrOne descendBi syb_incrOne"
  ,"isDept gmapT isDeptD com_incrOne uni_salaryBill syb_salaryBill billS com_salaryBill"]
  


main = do
    files <- getDirectoryContents "."
    (index,used) <- liftM concatUnzip $ mapM readTex $ filter ((==) ".tex" . takeExtension) files
    let bad = nub used \\ (boring ++ index)
    when (bad /= []) $
        putStrLn $ unwords bad


concatUnzip = (concat *** concat) . unzip


-- (index,code)
readTex :: FilePath -> IO ([String],[String])
readTex file = do
    src <- readFile file
    return $ concatUnzip $ map f $ tails src
    where
        f xs | "\\begin{code}" `isPrefixOf` xs = ([],readCode $ scanEnd $ drop 12 xs)
             | "\\ind{" `isPrefixOf` xs = ([takeWhile (/= '}') $ drop 5 xs],[])
             | otherwise = ([],[])


scanEnd xs | "\\end{code}" `isPrefixOf` xs = []
scanEnd (x:xs) = x : scanEnd xs


readCode xs = case lex $ dropWhile isSpace xs of
                  [("--",y)] -> readCode $ dropWhile (/= '\n') y
                  [("","")] -> []
                  [] -> []
                  [(x,y)] -> [x | good x] ++ readCode y
    where
        good (x:xs) = isAlpha x -- not $ x `elem` "'\"" || isDigit x
