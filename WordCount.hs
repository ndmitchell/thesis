{-
Could the number of words in the thesis

Needs to be custom since lhs2tex screws this up

Take two parameters
1) the location of the log file to append information to
2) the place to save the google graph to
-}

module Main(main) where

import Data.List
import Data.Char
import System.Environment
import System.Cmd
import System.Time
import Control.Monad


main = do
    args <- getArgs
    let nargs = length args
    count <- countFile "thesis.tex"
    putStrLn $ "Word count: " ++ show count

    when (nargs > 0) $ do
        d <- getDate
        appendFile (args !! 0) $ d ++ "\t" ++ show count ++ "\n"

    when (nargs > 1) $ do
        src <- readFile (args !! 0)
        let url = makeGraph $ parseLog src
        system $ "wget \"" ++ url ++ "\" -O " ++ (args !! 1)
        return ()


{-
FOLLOW: \include{file}
SKIP: commands.tex file
      anything between \begin{comment} ... \end{comment}
WORD: starts with a letter, ends with a space
      ignore any words prefixed with a \
-}

countFile :: FilePath -> IO Int
countFile file = do
    putStrLn $ "Counting: " ++ file
    src <- if file == "commands.tex" then return "" else readFile file
    sumM $ map countLine $ dropComments $ lines src

dropComments :: [String] -> [String]
dropComments (x:xs) | isBeginComment x = drop 1 $ dropWhile (not . isEndComment) xs
dropComments (x:xs) = x : dropComments xs
dropComments [] = []

isBeginComment = isPrefixOf "\\begin{comment}"
isEndComment   = isPrefixOf "\\end{comment}"

countLine x | "\\include{" `isPrefixOf` x = countFile $ takeWhile (/= '}') (drop 9 x) ++ ".tex"
countLine x = return $ f x
    where
        f ('\\':xs) = f (dropWhile isAlpha xs)
        f (x:xs) | isAlpha x = 1 + f (dropWhile (not . isSpace) xs)
        f (x:xs) = f xs
        f [] = 0



sumM :: [IO Int] -> IO Int
sumM = f 0
    where
        f i (x:xs) = do
            j <- x
            let i2 = i+j
            i2 `seq` f i2 xs

        f i [] = return i


type Date = (Int, Month, Int)

getDate :: IO String
getDate = do
    t <- getClockTime
    t <- toCalendarTime t
    return $ show $ (ctYear t, ctMonth t, ctDay t)


parseLog :: String -> [(Date, Int)]
parseLog = map f . lines
    where
        f x = (read a, read b)
            where (a,_:b) = break (== '\t') x


makeGraph :: [(Date,Int)] -> String
makeGraph dat = "http://chart.apis.google.com/chart" ++
                "?cht=lc" ++
                "&chs=400x300" ++
                "&chtt=Neil's+Thesis+Word+Count" ++
                "&chxt=x,y" ++
                "&chxl=0:|" ++ concatMap ((++ "|") . take 3 . show . snd) ranX ++
                      "1:" ++ concatMap (('|':) . show) ranY ++
                "&chd=s:" ++ simp
    where
        simp = map val rangeX

        (dates,values) = unzip dat
        minY = min 25000 (minimum values)
        maxY = max 30000 (maximum values)
        ranY = map (*1000) [minY `div` 1000 .. maxY `div` 1000]
        
        minX@(x1,x2,x3) = minimum dates
        maxX@(x4,x5,x6) = maximum $ (2008,March,10) : dates

        ranX = takeWhile (<= (x4,x5)) $ iterate next (x1,x2)
        next (i,December) = (i+1,January)
        next (i,j) = (i,succ j)
        
        rangeX = [(v1,v2,v3) | (v1,v2) <- ranX, v3 <- [1..31]]

        val d = encode $ scale $ snd $ head $
            (sortBy srt $ filter ((>= d) . fst) dat) ++ [(undefined,-1)]
        srt (a,b) (c,d) = (a, negate b) `compare` (c, negate d)

        scale y = (61 * (y - minY)) `div` (maxY - minY)

        encode i | i <  0  = '_'
                 | i <= 25 = chr $ i + ord 'A'
                 | i <= 51 = chr $ (i-26) + ord 'a'
                 | i <= 61 = chr $ (i-52) + ord '0'
                 | otherwise = error $ "encode with: " ++ show i
