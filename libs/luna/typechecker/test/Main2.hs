{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

{-# LANGUAGE PartialTypeSignatures     #-}

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prologue              hiding (cons, read)
import Luna.Syntax.AST.Term  hiding (Lit, Val, Thunk, Expr, Draft, Target, Source, source, target)
import qualified Luna.Syntax.AST.Term as Term
import Luna.Passes.Diagnostic.GraphViz
import Data.Layer.Cover
import Data.Record hiding (Layout)
import Luna.Syntax.AST.Layout (Static, Dynamic)
import Data.Attribute
import Luna.Syntax.Model.Graph
import Luna.Syntax.Model.Layer


renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)


title s = putStrLn $ "\n" <> "-- " <> s <> " --"

data IDT a = IDT a deriving (Show)


data MyGraph (t :: * -> *) = MyGraph deriving (Show)

type instance Layout (MyGraph t) term rt = t (Term (MyGraph t) term rt)

-- --------------------------------------
--  !!! KEEP THIS ON THE BEGINNING !!! --
-- --------------------------------------
-- - vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv ---
prebuild :: IO (Ref $ Node (NetLayers :< Draft Static), NetGraph)
prebuild = runNetworkBuilderT def $ star


data ImgAttr = ImgAttr deriving (Show)
type instance Attr ImgAttr (Cover x) = String

foo :: NetGraph -> IO (Ref $ Node (NetLayers :< Draft Static), NetGraph)
foo g = runNetworkBuilderT g
    $ do
    title "basic element building"
    s1 <- star
    s2 <- star
    print s1

    title "reading node references"
    s1_v <- read s1
    s2_v <- read s2
    print s1_v

    title "manual connection builing"
    c1 <- connection s1 s2

    title "reading connection references"
    c1_v <- read c1
    print c1_v

    title "edge following"
    c1_tgt <- follow c1
    when (c1_tgt /= s2) $ fail "reading is broken!"
    print "ok!"

    title "pattern matching"
    print $ uncover s1_v
    print $ caseTest (uncover s1_v) $ do
        match $ \Star -> "its a star! <3"
        match $ \ANY  -> "something else!"

    title "complex element building"
    u1 <- unify s1 s2
    print u1
    u1_v <- read u1

    title "inputs reading"
    let u1_ins = inputs (uncover u1_v)
    print u1_ins

    title "params reading"
    let s1t = s1_v ^. attr Type
        s1s = s1_v ^. attr Succs
        ca1 = case checkAttr Type of
            Just t  -> show $ s1_v ^. t
            Nothing -> "no type here!"
        --ca2 = case checkAttr ImgAttr of
        --    Just t  -> s1_v ^. t
        --    Nothing -> "no imagined attrib here!"
    print s1t
    print s1s
    print ca1
    -- print ca2


    return s1


main :: IO ()
main = do
    (star, g) <- prebuild
    print star
    print g
    putStrLn "\n--------------\n"
    (s,g') <- foo g
    print g'

    renderAndOpen [("g", g')]

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
