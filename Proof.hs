
module Main where

import Data.List
import Data.Maybe
import LazySmallCheck
import System.Environment


-- Property

data Prop a = Or [Prop a] | And [Prop a] | Lit a

andP = And
orP = Or
lit = Lit
true = And []


-- Constraints

data Sat a = Sat a Constraint

substP ::  Eq alpha => [(alpha,beta)] -> Prop (Sat alpha) -> Prop (Sat beta)
substP xs (Lit (Sat i k)) = Lit $ Sat (fromJust $ lookup i xs) k
substP xs (And p) = And $ map (substP xs) p
substP xs (Or p) = Or $ map (substP xs) p


-- MP constraints

type Constraint  =  [Val]
data Val         =  [Pattern] :* [Pattern] |  Any deriving (Show,Eq)
data Pattern     =  Pattern CtorName [Val] deriving (Show,Eq)


(<|) :: CtorName -> Constraint -> Prop (Sat Int)
c <| vs = orP (map f vs)
    where
    (rec,non) = partition (isRec . (,) c) [0..arity c-1]

    f Any = true
    f (ms_1 :* ms_2) = orP  [ andP $ map lit $ g vs_1
                            | Pattern c_1 vs_1 <- ms_1, c_1 == c]
        where g vs =  zipWith Sat non (map (:[]) vs) ++
                      map (`Sat` [ms_2 :* ms_2]) rec

mergeVal :: Val -> Val -> Val
(a_1 :* b_1)  `mergeVal`  (a_2 :* b_2)  = merge a_1 a_2 :* merge b_1 b_2
x             `mergeVal`  y             = if x == Any then y else x

merge :: [Pattern] -> [Pattern] -> [Pattern]
merge  ms_1 ms_2 = [Pattern c_1 (zipWith mergeVal vs_1 vs_2) |
       Pattern c_1 vs_1 <- ms_1, Pattern c_2 vs_2 <- ms_2, c_1 == c_2]


-- Evaluator

data Value  =  Value CtorName [Value]
            |  Bottom

sat :: Sat Value -> Bool
sat (Sat Bottom        k) = True
sat (Sat (Value c xs)  k) = sat' $ substP (zip [0..] xs) (c <| k)

sat' :: Prop (Sat Value) -> Bool
sat' (And xs) = all sat' xs
sat' (Or xs) = any sat' xs
sat' (Lit x) = sat x


-- Core language

data CtorName = Ctor | CtorN | CtorR | CtorNR | CtorRN
                deriving (Show,Eq)

arity Ctor = 0
arity CtorN = 1
arity CtorR = 1
arity CtorNR = 2
arity CtorRN = 2

isRec (CtorR,  0) = True
isRec (CtorNR, 1) = True
isRec (CtorRN, 0) = True
isRec _ = False

data CtorVal = Ctor_ | CtorN_ CtorVal | CtorR_ CtorVal | CtorNR_ CtorVal CtorVal | CtorRN_ CtorVal CtorVal
               deriving (Show,Eq)

conv (Ctor_) = Value Ctor []
conv (CtorN_ a) = Value CtorN [conv a]
conv (CtorR_ a) = Value CtorR [conv a]
conv (CtorNR_ a b) = Value CtorNR [conv a, conv b]
conv (CtorRN_ a b) = Value CtorRN [conv a, conv b]


-- Generators

instance Serial CtorVal where
    series = cons0 Ctor_ \/ cons1 CtorN_ \/ cons1 CtorR_ \/ cons2 CtorNR_ \/ cons2 CtorRN_

instance Serial CtorName where
    series = cons0 Ctor \/ cons0 CtorN \/ cons0 CtorR \/ cons0 CtorNR \/ cons0 CtorRN

instance Serial Val where
    series = cons2 (:*) \/ cons0 Any

instance Serial Pattern where
    series = cons2 Pattern


-- Property

prop :: CtorVal -> [Pattern] -> [Pattern] -> Bool
prop v ms1 ms2 = sat (Sat v2 [ms :* ms]) ==> sat (Sat v2 [ms1 :* ms2])
    where
        v2 = conv v
        ms = merge ms1 ms2


main :: IO ()
main = do
    [x] <- getArgs
    depthCheck (read x) prop
