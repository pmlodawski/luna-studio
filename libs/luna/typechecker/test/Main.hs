{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

{-# LANGUAGE PartialTypeSignatures     #-}

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prologue             hiding (cons)
import Luna.Syntax.AST.Term hiding (Arrow, Node)
import qualified Luna.Syntax.AST.Term2 as T2
import Tmp
import Luna.Passes.Diagnostic.GraphViz
import Data.Layer.Cover
import Data.Record
import qualified Luna.Syntax.AST.Layout as Runtime


renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)


title s = putStrLn $ "\n" <> "-- " <> s <> " --"

main :: IO ()
main = do


    g <- buildNetworkM $ do
        title "basic element building"
        (s1 :: _) <- star
        (s2 :: _) <- star
        print s1

        title "reading node references"
        (s1_v :: _) <- readRef s1
        (s2_v :: _) <- readRef s2
        print (uncover s1_v :: _)

        title "manual connection builing"
        (c1 :: _) <- connection s1 s2
        print c1

        title "reading connection references"
        c1_v <- readRef c1
        print c1_v

        title "edge following"
        c1_tgt <- follow c1_v
        when (c1_tgt /= s2_v) $ fail "reading is broken!"

        title "pattern matching"
        print $ caseTest (uncover s1_v) $ do
            match $ \(Lit l)  -> caseTest l $ do
                match $ \Star -> "its a star! <3"
                match $ \ANY  -> "some literal"
            match $ \ANY      -> "something else!"

        title "complex element building"
        u1 <- unify s1 s2
        print u1
        u1_v <- readRef u1

        title "inputs reading"
        let u1_ins = inputs (uncover u1_v)
        print u1_ins

        title "params reading"
        let s1t = s1_v ^. (access Type)
            s1s = s1_v ^. (access Successors)
        print s1t
        print s1s


        return ()



    renderAndOpen [("g", g)]

    return ()


-------------------------
-- === Benchmarks === ---
-------------------------


--data Bench a = Bench1 a
--             | Bench2
--             deriving (Show)

--main = do


--    args <- getArgs
--    let mode   = read (args !! 0) :: Int
--        argnum = read (args !! 1) :: Int
--        nums = [0..argnum]


--    case mode of
--        0 -> do
--            let ls = const star . show <$> nums
--                pattest l = caseTest l $ do
--                    variantMatch (\Star -> (1 :: Int))
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
--            --print $ sum $ getnum <$> ls
--        1 -> do
--            let ls = const Bench2 . show <$> nums
--                pattest l = case l of
--                    Bench2 -> (1 :: Int)
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
--            --print $ sum $ getnum <$> ls


-- === Performance notes === ---
-- Performance drops observed:
--     - using custom State class and a wrapper for pattern-matches causes drop
--       probably because automatically derived methods in the State wrapper are not inlined (TBI).
--     - using the `reverse` function in pattern match causes a drop, but it should be computed always during the compile time.