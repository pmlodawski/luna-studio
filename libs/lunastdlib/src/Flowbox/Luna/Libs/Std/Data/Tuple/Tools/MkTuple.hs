{-# LANGUAGE PatternGuards #-}
module Main where
import Data.List
import System.Environment

import           Data.String.Utils                 (join)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [w, sn] | Just gen <- lookup w gens, [(n,"")] <- reads sn -> do
            putStrLn "--snip-----------------"
            putStrLn "---- Machine generated code below, see Tools/MkTuple.hs"
            putStrLn $ "---- " ++ unwords ("mkTuple" : args)
            gen n
        _ -> error $ "Usage: MkTuple generator number\n"

gens :: [(String, Int -> IO ())]
gens = [("select", generateSel),
        ("sequence", generateSeq),
        ("curry", generateCurry),
        ("update", generateUpd)
       ]

---------

generateSel :: Int -> IO ()
generateSel n = do 
    putStrLn "{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}\n"
    putStrLn "module Flowbox.Luna.Libs.Std.Data.Tuple.Select where"
    putStrLn $ join "\n" (map (("import Flowbox.Luna.FClasses.C''select" ++) . (++"'") . show) [0..n])
    putStrLn "import Data.Tuple.OneTuple\n"
    putStrLn "instance C''select0' (OneTuple a) a where select0' (OneTuple x) = x; select0'''M = return . select0'\n"
    mapM_ (generateSelN (n+1)) [0..n]

generateSelN :: Int -> Int -> IO ()
generateSelN n i = do
    -- putStrLn $ "class Sel" ++ show i ++ " a b | a -> b where sel" ++ show i ++ " :: a -> b"
    --putStrLn $ "import Common'.C''select" ++ show i
    mapM_ (generateSelNinst i) [i..n-1]
    putStrLn ""

generateSelNinst :: Int -> Int -> IO ()
generateSelNinst 0 0 = return ()
generateSelNinst j i = do
    putStrLn $ "instance C''select" ++ show j ++ "' (" ++ intercalate "," ["a" ++ show l | l <- [0..i]] ++ ") a" ++
               show j ++ " where select" ++ show j ++ "' (" ++ 
               intercalate "," [if l == j then "x" else "_" | l <- [0..i]] ++ ") = x"
               ++ "; "
               ++ "select" ++ show j ++ "'''M = return . select" ++ show j ++ "'"

---------

generateSeq :: Int -> IO ()
generateSeq n = mapM_ generateSeqN [2..n]

generateSeqN :: Int -> IO ()
generateSeqN i =
    putStrLn $ "instance (Monad m) => SequenceT (" ++
               intercalate "," ["m a" ++ show j | j <- [1..i]] ++ ") (m (" ++
               intercalate "," ["a" ++ show j | j <- [1..i]] ++ ")) where sequenceT (" ++
               intercalate "," ["a" ++ show j | j <- [1..i]] ++ ") = return (" ++ replicate (i-1) ',' ++ ") `ap` " ++
               intercalate " `ap` " ["a" ++ show j | j <- [1..i]]

---------

generateCurry :: Int -> IO ()
generateCurry n = mapM_ generateCurryN [2..n]

generateCurryN :: Int -> IO ()
generateCurryN i =
    putStrLn $ "instance Curry (" ++ tup ++ " -> r) (" ++
               intercalate "->" vars ++ " -> r) where\n" ++
               "    curryN f " ++ varsp ++ " = f " ++ tup ++ "\n" ++
               "    uncurryN f ~" ++ tup ++ " = f " ++ varsp
  where vars = ["a" ++ show j | j <- [1..i]]
        tup = "(" ++ intercalate "," vars ++ ")"
        varsp = unwords vars

---------

generateUpd :: Int -> IO ()
generateUpd n = mapM_ (generateUpdN n) [1..n]

generateUpdN :: Int -> Int -> IO ()
generateUpdN n i = do
    putStrLn $ "class Upd" ++ show i ++ " a b c | a b -> c , b c -> a where upd" ++ show i ++ " :: a -> b -> c"
    mapM_ (generateUpdNinst i) [i..n]
    putStrLn ""

generateUpdNinst :: Int -> Int -> IO ()
generateUpdNinst 1 1 = return ()
generateUpdNinst j i = do
    putStrLn $ "instance Upd" ++ show j ++ " b (" ++ intercalate "," ["a" ++ show l | l <- [1..i]] ++ ") " ++
               res ++ " where upd" ++ show j ++ " b (" ++ 
               intercalate "," [ "a" ++ show l | l <- [1..i]] ++ ") = " ++ res
 where res =
        "(" ++ intercalate "," [ if l == j then "b" else "a" ++ show l | l <- [1..i]] ++ ")"
