-- Lazy SmallCheck (typed, type-class variant)
-- Lindblad, Naylor and Runciman

module LazySmallCheck
  ( Cons                 -- :: *
  , Decons               -- :: *
  , Refine(cons,decons)  -- :: class
  , Apply(fun,(><))      -- :: class
  , (><*)                -- :: Refine a => Decons (a -> b) -> a -> Decons b
  , (\/)                 -- :: Cons a -> Cons a -> Cons a
  , depth                -- :: (Int -> Int) -> Decons a -> Decons a
  , series               -- :: (Int -> [a]) -> Cons a
  , var                  -- :: Cons a
  , child                -- :: Refine a => Cons a
  , decons0              -- :: a -> Decons a
  , decons1              -- :: (a -> b) -> a -> Decons b
  , decons2              -- :: (a -> b -> c) -> a -> b -> Decons c
  , decons3              -- :: (a -> b -> c -> d) -> a -> b -> c -> Decons d
  , decons4              -- :: (a -> b -> c -> d -> e) -> ...
  , cons0                -- :: a -> Cons a
  , cons1                -- :: (a -> b) -> Cons b
  , cons2                -- :: (a -> b -> c) -> Cons c
  , cons3                -- :: (a -> b -> c -> d) -> Cons d
  , cons4                -- :: (a -> b -> c -> d -> e) -> Cons e
  , cons0'               -- :: a -> Cons a
  , cons1'               -- :: (a -> b) -> Cons b
  , cons2'               -- :: (a -> b -> c) -> Cons c
  , cons3'               -- :: (a -> b -> c -> d) -> Cons d
  , cons4'               -- :: (a -> b -> c -> d -> e) -> Cons e
  , Property(depthCheck) -- :: class
  , test                 -- :: (Refine a,Show a,Property b) => (a->b) -> IO ()
  , (==>)                -- :: Bool -> Bool -> Bool
  , Prop                 -- :: *
  , lift                 -- :: Bool -> Prop
  , neg                  -- :: Prop -> Prop
  , (*&*)                -- :: Prop -> Prop -> Prop
  , (*|*)                -- :: Prop -> Prop -> Prop
  , (*=>*)               -- :: Prop -> Prop -> Prop
  ) where

import Monad
import Control.Exception
import System.Exit

infixr 0 ==>, *=>*
infixr 3 *|*
infixl 4 ><, ><*, *&*

-- Positions

type Pos = [Int]

toPos :: String -> Pos
toPos = map (negate . fromEnum)

fromPos :: Pos -> String
fromPos = map (toEnum . negate)

-- Traversal

class Apply a where
  fun :: x -> a x
  (><) :: a (x -> y) -> a x -> a y

-- Refinement

newtype Decons a = D { runD :: Int -> Pos -> Pos -> [a] }

newtype Cons a = C { runC :: Int -> Int -> Pos -> [a] }

class Refine a where
  decons :: a -> Decons a
  cons :: Cons a

refine :: Refine a => a -> Decons a
refine a = D ref
  where
    ref d [0] p = runC cons 0 d p
    ref d (i:is) p = if i == 0 then runD (decons a) d is p else [a]

-- Deconstruction

instance Apply Decons where
  fun a = D $ \d is p -> [a]
  f >< a = D $ \d (i:is) p ->
             [c b | c <- runD f d (i+1:is) p, b <- runD a (d-1) (i:is) p]

depth :: (Int -> Int) -> Decons a -> Decons a
depth f a = D $ \d is p -> runD a (f d) is p

(><*) :: Refine a => Decons (a -> b) -> a -> Decons b
f ><* a = f >< refine a

decons0 f = fun f
decons1 f a = fun f ><* a
decons2 f a b = fun f ><* a ><* b
decons3 f a b c = fun f ><* a ><* b ><* c
decons4 f a b c d = fun f ><* a ><* b ><* c ><* d

-- Construction

instance Apply Cons where
  fun a = C $ \i d p -> [a]
  f >< x = C $ \i d p -> [g y | g <- runC f (i-1) d p, y <- runC x i d p]

(\/) :: Cons a -> Cons a -> Cons a
a \/ b = C $ \i d p -> runC a i d p ++ runC b i d p

series :: (Int -> [a]) -> Cons a
series f = C $ \i d p -> f d

var :: Cons a
var = C $ \i d p -> [error $ fromPos (p++[i]) | d > 0]

child :: Refine a => Cons a
child = C $ \i d p -> if d >= 0 then runC cons 0 (d-1) (p++[i]) else []

cons0 c = fun c
cons1 c = fun c >< var
cons2 c = fun c >< var >< var
cons3 c = fun c >< var >< var >< var
cons4 c = fun c >< var >< var >< var >< var

cons0' c = fun c
cons1' c = fun c >< child
cons2' c = fun c >< child >< child
cons3' c = fun c >< child >< child >< child
cons4' c = fun c >< child >< child >< child >< child

-- Refining algebraic types

instance Refine () where
  cons = cons0 ()

instance Refine Bool where
  cons = cons0 False \/ cons0 True

instance Refine a => Refine (Maybe a) where
  decons (Just x) = decons1 Just x
  cons = cons0 Nothing \/ cons1 Just

instance (Refine a, Refine b) => Refine (Either a b) where
  decons (Left x) = decons1 Left x
  decons (Right x) = decons1 Right x
  cons = cons1 Left \/ cons1 Right

instance Refine a => Refine [a] where
  decons (x:xs) = decons2 (:) x xs
  cons = cons0 [] \/ cons2 (:)

-- Refining tuples (no depth-cost to refine a tuple)

instance (Refine a, Refine b) => Refine (a, b) where
  decons (a,b) = depth (+1) $ decons2 (,) a b
  cons = cons2 (,)

instance (Refine a, Refine b, Refine c) => Refine (a,b,c) where
  decons (a,b,c) = depth (+1) $ decons3 (,,) a b c
  cons = cons3 (,,)

instance (Refine a, Refine b, Refine c, Refine d) => Refine (a,b,c,d) where
  decons (a,b,c,d) = depth (+1) $ decons4 (,,,) a b c d
  cons = cons4 (,,,)

-- Refining primitive types, a la SmallCheck

instance Refine Char where
  cons = series $ \d -> take (d+1) ['a'..'z']

instance Refine Int where
  cons = series $ \d -> [(-d)..d]

instance Refine Integer where
  cons = series $ \d -> map toInteger [(-d)..d]

floats d = [ encodeFloat sig exp
           | sig <- map toInteger [(-d)..d]
           , exp <- [(-d)..d]
           , odd sig || sig == 0 && exp == 0 ]

instance Refine Float where
  cons = series floats

instance Refine Double where
  cons = series $ \d -> map frac (floats d)

frac :: (Real a, Fractional a, Real b, Fractional b) => a -> b
frac = fromRational . toRational

-- Answers

answer :: a -> (a -> IO b) -> (Pos -> IO b) -> IO b
answer a known unknown =
  do res <- try (evaluate a)
     case res of
       Right b -> known b
       Left (ErrorCall p) -> unknown (toPos p)
       Left e -> throw e

-- Property refutation

refute :: (Refine a, Show a) => Int -> (a -> Prop) -> a -> IO Int
refute d f x = eval (f x) known unknown
  where
    known True = return 1
    known False = display d x >> exitWith ExitSuccess
    unknown p = sumMapM (refute d f) 1 (runD (refine x) d p p)

sumMapM :: (a -> IO Int) -> Int -> [a] -> IO Int
sumMapM f n [] = return n
sumMapM f n (a:as) = seq n (do m <- f a ; sumMapM f (n+m) as)

-- Properties with parallel conjunction (Lindblad TFP'07)

data Prop = Bool Bool | Neg Prop | And Prop Prop | ParAnd Prop Prop

eval :: Prop -> (Bool -> IO a) -> (Pos -> IO a) -> IO a
eval p k u = answer p (\p -> eval' p k u) u

eval' (Bool b) k u = answer b k u
eval' (Neg p) k u = eval p (k . not) u
eval' (And p q) k u = eval p (\b -> if b then eval q k u else k b) u
eval' (ParAnd p q) k u = eval p (\b -> if b then eval q k u else k b) unknown
  where
    unknown pos = eval q (\b -> if b then u pos else k b) (\_ -> u pos)

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> x = x

lift :: Bool -> Prop
lift b = Bool b

neg :: Prop -> Prop
neg p = Neg p

(*&*), (*|*), (*=>*) :: Prop -> Prop -> Prop
p *&* q = ParAnd p q
p *|* q = neg (neg p *&* neg q)
p *=>* q = neg p *|* q

-- Top-level

class Property p where
  depthCheck :: (Refine a, Show a) => Int -> (a -> p) -> IO ()

instance Property Bool where
  depthCheck d p = depthCheck d (Bool . p)

instance Property Prop where
  depthCheck d p =
    do n <- refute d p (error [toEnum 0])
       putStrLn $ "Passed " ++ show n ++ " tests at depth " ++ show d ++ "."

test :: (Refine a, Show a, Property b) => (a -> b) -> IO ()
test p = mapM_ (`depthCheck` p) [0..]

-- Display a counter example in fully-defined form

display :: (Refine a, Show a) => Int -> a -> IO ()
display d x = putStr "Counter example found" >> display' d [x]
  where
    display' _ [] = putStrLn ", but out of depth."
    display' d (x:xs) = answer (demand (show x)) known unknown
      where
        known _ = putStrLn ":" >> print x
        unknown p = display' d (xs ++ runD (refine x) d p p)

    demand [] = ()
    demand (x:xs) = seq x (demand xs)
