import Control.Monad
import Control.Applicative
import            Data.Map.Strict             (Map)
import qualified  Data.Map.Strict             as M
import Data.List                        (sort,nub,sortBy,intercalate)
import Data.Ord                         (comparing)
import Data.Tuple                       (swap)
import Debug.Trace
import Data.Maybe
import Debug.Trace



newtype AlphaEquivMonad a
  = AlphaEquivMonad { runAlphaEq  :: Int -> Int -> [ (a, Int, Int) ] }



instance Monad AlphaEquivMonad where
    fail _ = AlphaEquivMonad aux
      where aux ttab ttba = []

    return x = AlphaEquivMonad aux
      where aux ttab ttba = [(x,ttab,ttba)]

    m >>= ak = AlphaEquivMonad aux
      where aux ttab ttba = go `concatMap` runAlphaEq m ttab ttba
            go (x, ttab, ttba) = runAlphaEq (ak x) ttab ttba

instance Functor AlphaEquivMonad where
    fmap f m = AlphaEquivMonad aux
      where aux ttab ttba = go <$> runAlphaEq m ttab ttba
            go (x, ttab, ttba) = (f x, ttab, ttba)
instance Applicative AlphaEquivMonad where
    pure = return
    (<*>) = ap


fun1 x = AlphaEquivMonad aux
  where aux ta tb = [(x * 10, ta, tb)]

fun2 x = AlphaEquivMonad aux
  where aux ta tb = [(x * 100, ta, tb)]

fun3 x = AlphaEquivMonad aux
  where aux ta tb = [(x * 1000, ta, tb)]


fun4 = AlphaEquivMonad aux
  where aux ta tb = [((), ta - 3, tb - 5)]

fun5 = AlphaEquivMonad aux
  where aux ta tb = [((), ta - 3, tb + 5)]

fun6 = AlphaEquivMonad aux
  where aux ta tb = [((), ta + 3, tb - 5)]

fun7 = AlphaEquivMonad aux
  where aux ta tb = [((), ta + 3, tb + 5)]


getta = AlphaEquivMonad aux
  where aux ta tb = [(ta,ta,tb)]

gettb = AlphaEquivMonad aux
  where aux ta tb = [(tb,ta,tb)]


fork :: [AlphaEquivMonad ()] -> AlphaEquivMonad ()
fork fs = AlphaEquivMonad aux
  where aux ttab ttba = concat [runAlphaEq f ttab ttab | f <- fs]

go :: AlphaEquivMonad Int
go = do
  return 5
  fork [fun4, fun5, fun6, fun7]
  x <- getta
  y <- gettb
  fork [fun4, fun5, fun6, fun7]
  return (x + y)






main = do
  putStrLn "dupa"
  print $ runAlphaEq go 0 0

