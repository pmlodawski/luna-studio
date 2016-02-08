{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds     #-}

module Main where

import Prologue              hiding (cons, read, (#))
import Luna.Syntax.AST.Term  hiding (Lit, Val, Thunk, Expr, Draft, Target, Source, source, target)
import qualified Luna.Syntax.AST.Term as Term
import Luna.Diagnostic.Vis.GraphViz
import Data.Layer.Cover
import Data.Record hiding (Layout)
import Luna.Evaluation.Runtime (Static, Dynamic)
import qualified Luna.Evaluation.Runtime as Runtime
import Luna.Syntax.Model.Graph
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder
import Luna.Syntax.Model.Network.Term
import Data.Construction
import Luna.Syntax.Model.Graph.Builder.Ref as Ref
import Data.Prop
import Data.Graph.Sort hiding (Graph)
import qualified Data.Graph.Sort as Sort
import Development.Placeholders
import Data.Container  (index_, elems)
import Data.Index (idx)
import Data.Container
import           Data.Attr (attr)
import           Control.Monad.Event
import           Luna.Syntax.Model.Network.Builder.Node hiding (star, str,int,cons,acc,app,var,unify,blank)
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Node.Class ()
import Type.Inference

import qualified Luna.Compilation.Passes.Inference.Struct as S

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
prebuild :: Show a => IO (Ref $ Node (NetLayers a :< Draft Static), NetGraph a)
prebuild = runNetworkBuilderT def $ star


type instance Item (NetGraph a) = Ref $ Node (NetLayers a :< Draft Static)

instance Sort.CompleteGraph     (Graph (NetLayers a :< Raw) (Link (NetLayers a :< Raw)))

instance Sort.MarkableGraph     (Graph (NetLayers a :< Raw) (Link (NetLayers a :< Raw))) where
    markNode ref g = snd $ rebuildNetwork' g $ do
        Ref.with ref $ prop Markable .~ True
    isMarked ref g = fst $ rebuildNetwork' g $ do
        node <- read ref
        return $ node # Markable

instance Sort.Graph             (Graph (NetLayers a :< Raw) (Link (NetLayers a :< Raw))) where
    listNodes g = map (Ref . Ptr) $ (usedIxes $ g ^. nodeGraph)

instance Sort.ForwardEdgedGraph (Graph (NetLayers a :< Raw) (Link (NetLayers a :< Raw))) where
    successors ref g = fst $ rebuildNetwork' g $ do
        node <- read ref
        mapM (follow target) $ node ^. prop Succs


foo :: forall a. Show a => NetGraph a -> IO (Ref $ Node (NetLayers a :< Draft Static), NetGraph a)
--foo :: NetGraph -> IO ((), NetGraph)
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
    c1_tgt <- follow target c1
    when (c1_tgt /= s2) $ fail "reading is broken!"
    print "ok!"

    title "pattern matching"
    print $ uncover s1_v
    print $ caseTest (uncover s1_v) $ do
        match $ \Star -> "its a star! <3"
        match $ \ANY  -> "something else!"

    title "complex element building"
    u1 <- unify s1 s2
    print (u1 :: Ref $ Node (NetLayers a :< Draft Static))
    u1_v <- read u1

    title "inputs reading"
    let u1_ins = u1_v # Inputs
    print u1_ins

    title "params reading"
    let s1t = s1_v # Type
        s1s = s1_v # Succs
    print s1t
    print s1s

    return s1


infTest :: ( MonadIO        m
           , TermNode Star  m (ls :< term)
           , TermNode Unify m (ls :< term)
           , NodeInferable  m (ls :< term)
           , LitLike          (ls :< term)
           ) => m ()
infTest = do
    infTest
    s1 <- Inf.star
    s2 <- Inf.star
    u1 <- Inf.unify s1 s2
    s1_v <- read s1
    write s1 s1_v
    let x = uncover s1_v
    print $ caseTest x $ do
        match $ \Star -> "its a star! <3"
        match $ \ANY  -> "something else!"
    return ()



runInfTest :: forall a. Show a => NetGraph a -> IO ((), NetGraph a)
runInfTest g = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers a :< Draft Static)))
             $ runNetworkBuilderT g infTest





main :: IO ()
main = do
    --(star, g :: NetGraph ()) <- prebuild
    --print star
    --print g
    --putStrLn "\n--------------\n"
    --(s,g') <- foo g
    --print g'

    --title "graph sorting"
    --print $ Sort.sortBy (const True) g'

    --title "params reading"
    --print $ g' # s

    --renderAndOpen [("g", g')]

    S.main

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
