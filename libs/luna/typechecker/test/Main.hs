{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

{-# LANGUAGE PartialTypeSignatures     #-}

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prologue
import Luna.Syntax.AST.Term hiding (Arrow, Node)
import Tmp
import Luna.Passes.Diagnostic.GraphViz
import Data.Layer.Cover


renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)



main :: IO ()
main = do

        --s = star
    --g <- flip evalStateT (0 :: Int) $ flip Graph.execT (def :: Network) $ do
       -- $ constrainCoverType (Proxy :: Proxy (Ref' Node (Attached' String Cover)))

    g <- buildNetworkM $ do
        --x <- constructCover star :: _ (TargetRef Node (Attached' String Cover) (Lit (TargetRef Edge  (Attached' String Cover))))


        --let x = undefined :: Node2 (Static Draft :> '[])
        --let x = undefined :: AttachmentCover '[Type] (Static Draft IDT)


        --print $ typeOf x
        --print x

        --(s1 :: Ref2 (Node2 (Static Draft :> '[]))) <- starx' 
        (s1 :: _) <- starx' 
        --s2 <- star' 
        print s1
        --print (s2 :: _)

        ----s3 <- star'
        (s1_v :: _) <- readRef s1
        print (uncover s1_v :: _)

        --caseTest s1_v $ do
        --    match $ \()
        --print (s1_v :: NetCover (Static Draft (RefCover Edge NetCover (Static Draft))))
        --starx'
        --print (s1 :: Netref Node (Static Draft))
        return ()


--Ref $ Node $ Static Draft :> '[Type, Succ]
--Ref $ Link $ Static Draft :> '[Type, Succ]



--Ref Node '[Type, Succ] (Static Draft (Ref Node '[Type, Succ]))

--Node '[Type, Succ] (Static Draft (Ref Node '[Type, Succ]))


--Ref Node (Layered '[Type, Succ] (Static Draft ))

--MuRef Node '[Type,Succ] (Static Draft)

--        s2 <- starx'
        --print (s1 :: Netref Node (Static Draft))


--        c <- connection s1 s2
--        print c


--        s1_v <- readRef s1
--        c_v <- readRef c

--        print s1_v
--        print c_v

--        uu <- unifyx' s1 s2

----        --print (withElement' (p :: P MyShow) myShow $ uncover s1_v)
--        uu_v <- readRef uu


--        print $ view (access Type) uu_v
----        --print (uncover uu_v :: Static Draft (RefCover Edge (Attached' Type (Netref Node (Static Draft)) Cover) (Static Draft)))
----        --print $ withElement_ (p :: P (TFoldable (Static Draft (RefCover Edge (Attached' Type (Netref Node (Static Draft)) Cover) (Static Draft))))) (foldrT (:) []) uu_v
----        --u <- unifyx' s1 s2
------class WithElement_ ctx rec where withElement_ :: Proxy ctx -> (forall v. ctx v => v -> a) -> rec -> a

--        print $ (inputs $ uncover uu_v)

----        --print $ ucase s1_v $ do
----        --    match $ \(Lit _) -> "Its a Lit!"
----        --    match $ \ANY     -> "Something else"


        --return ()

        --s' = s & coated %~ unwrap' ∘ unwrap'

    --print $ (g :: Network)
    return ()


    renderAndOpen [("g", g)]

    --print $ caseTest t1 $ do
    --    --match $ \Star    -> "star!"
    --    --dynamic $ \s -> "its dynamic! :O"
    --    static $ \s -> "it is static!  :O"
    --    match  $ \(Cons _ _) -> "its cons ..."
    --    --match  $ \(Lit l) -> caseTest l $ do
    --    --    match $ \Star -> "its star!"
    --    match $ \ANY     -> "something else"
    



--    return ()


-- time  take  -  description                              FIXME
----------------------------------------------------------------
--           [+] readRef dla Edge               
-- 0:35  30  [+] automatic constructed connection type
-- 2:18  30  [?] types
-- 4:11  30  [+] predecessors
--       30  [+] attach accessors
--       30  [ ] successors
--       30  [ ] destructors
--       30  [ ] term construction methods
--       30  [ ] nice connect / reconnect
--
-- [ ] magic monad builder
-- [ ] pretty TH case
-- [ ] 



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