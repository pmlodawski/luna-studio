{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

-- import Prologue hiding (Cons, Indexable, Ixed, Repr, Simple, children, cons, empty, index, lookup, maxBound, minBound, repr, s, simple)

-- import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.Identity
-- import qualified Data.GraphViz.Attributes            as GV
-- import qualified Data.GraphViz.Attributes.Colors.X11 as GVC
-- import           Data.Layer.Coat
-- import qualified Data.Map                            as Map
-- import           Data.Variants                       hiding (cons)
-- import           Data.Vector.Mutable                 ()
-- import           Debug.Trace
-- import           Development.Placeholders

-- import           Luna.Diagnostic.AST          as Diag (LabelAttrs (..), open, render, toGraphViz)
-- import qualified Luna.Inference.Literals      as Literals
-- import qualified Luna.Inference.Applications  as Applications
-- import qualified Luna.Interpreter.Interpreter as Interpreter
-- import           Luna.Interpreter.Label       (Label)
-- import qualified Luna.Interpreter.Label       as Label
-- import qualified Luna.Interpreter.Monad       as InterpreterMonad
-- import qualified Luna.Interpreter.NodeRunner  as NodeRunner
-- import qualified Luna.Interpreter.Session     as Session
-- import           Luna.Syntax.AST.Term
-- import           Luna.Syntax.AST.Arg
-- import           Luna.Syntax.AST.Typed        (Typed)
-- import           Luna.Syntax.Builder
-- import qualified Luna.Syntax.Builder          as Builder
-- import qualified Luna.Syntax.Builder.Node     as NodeBuilder
-- import qualified Luna.Syntax.Builder.Star     as StarBuilder
-- import qualified Luna.Syntax.Builder.Symbol   as SymbolBuilder
-- import           Luna.Syntax.Layer.Labeled    (label)
-- import           Luna.Syntax.Layer.Labeled    (Labeled2)
-- import           Luna.Syntax.Repr.Graph
-- import           Luna.Syntax.Symbol.Map       (SymbolMap)
-- import qualified Luna.Syntax.Symbol.Map       as Symbol
-- import           Luna.Syntax.Network          (Network)


import           Data.Attribute
import           Data.Layer.Cover
import           Data.Record                     hiding (Layout)
import           Luna.Passes.Diagnostic.GraphViz
import           Luna.Syntax.AST.Layout          (Dynamic, Static)
import           Luna.Syntax.AST.Term            hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import qualified Luna.Syntax.AST.Term            as Term
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Layer
import           Prologue                        hiding (cons, read)
import           Tmp2


-- ====================================

-- typed a t = StarBuilder.with (const $ Just t) a

-- renderAndOpen lst = do
--     flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
--     open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)

-- instance LabelAttrs (Labeled2 Label (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) where
--     labelAttrs n = []
--         -- if n ^. label . Label.checked then [GV.color GVC.Magenta]
--         --                               else []

-- ====================================






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
prebuild = rebuildNetworkM def $ star


data ImgAttr = ImgAttr deriving (Show)
type instance Attr ImgAttr (Cover x) = String


-- emptyArgList :: [Arg (Ref Node)]
-- emptyArgList = []

-- emptyNodeList :: [Ref Node]
-- emptyNodeList = []

-- sampleGraph1 :: ((Ref Node, SymbolMap (Network Label (Maybe Int))), Network Label (Maybe Int))
-- sampleGraph1 = runIdentity
--       $ flip StarBuilder.evalT Nothing
--       $ flip Builder.runT def
--       $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
--       $ do
--             nameInt       <- _string "Int"
--             nameString    <- _string "String"
--             namePlus      <- _string "+"
--             nameConc      <- _string "++"
--             nameLen       <- _string "len"

--             consIntTpe    <- cons nameInt
--             consStringTpe <- cons nameString

--             arrPlusTpe    <- arrow [consIntTpe] Map.empty consIntTpe
--             arrConcTpe    <- arrow [consStringTpe] Map.empty consStringTpe
--             arrLenTpe     <- arrow emptyNodeList Map.empty consIntTpe

--             i1 <- _int 2 -- `typed` arrLenTpe
--             i2 <- _int 3
--             i3 <- _int 4
--             s1 <- _stringVal "abc" -- `typed` consStringTpe
--             s2 <- _stringVal "def"
--             s3 <- _stringVal "ghi"

--             accPlus1a  <- accessor namePlus i1
--             appPlus1a  <- app accPlus1a [arg i2] `typed` arrPlusTpe

--             accPlus1b  <- accessor namePlus i3
--             appPlus1b  <- app accPlus1b [arg appPlus1a] `typed` arrPlusTpe

--             accConc1a  <- accessor nameConc s2
--             appConc1a  <- app accConc1a [arg s1] `typed` arrConcTpe

--             accConc1b  <- accessor nameConc appConc1a
--             appConc1b  <- app accConc1b [arg s3] `typed` arrConcTpe

--             accLen    <- accessor nameLen appConc1b
--             appLen    <- app accLen emptyArgList `typed` arrLenTpe

--             accPlus2  <- accessor namePlus appPlus1b
--             appPlus2  <- app accPlus2 [arg appLen] `typed` arrPlusTpe

--             return (appPlus1b, def)
--             -- return (appPlus2, def)

-- sampleGraph2 :: ((Ref Node, SymbolMap (Network Label (Maybe Int))), Network Label (Maybe Int))
-- sampleGraph2 = runIdentity
--       $ flip StarBuilder.evalT Nothing
--       $ flip Builder.runT def
--       $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
--       $ do
--             -- nameInt       <- _string "Int"
--             -- nameString    <- _string "String"
--             namePlus      <- _string "+"
--             nameConc      <- _string "++"
--             nameLen       <- _string "len"

--             i1 <- _int 2
--             i2 <- _int 3
--             i3 <- _int 4
--             s1 <- _stringVal "abc"
--             s2 <- _stringVal "def"
--             s3 <- _stringVal "ghi"

--             accPlus1a  <- accessor namePlus i1
--             appPlus1a  <- app accPlus1a [arg i2]

--             accPlus1b  <- accessor namePlus i3
--             appPlus1b  <- app accPlus1b [arg appPlus1a]

--             accConc1a  <- accessor nameConc s2
--             appConc1a  <- app accConc1a [arg s1]

--             accConc1b  <- accessor nameConc appConc1a
--             appConc1b  <- app accConc1b [arg s3]

--             accLen    <- accessor nameLen appConc1b
--             appLen    <- app accLen emptyArgList

--             accPlus2  <- accessor namePlus appPlus1b
--             appPlus2  <- app accPlus2 [arg appLen]

--             return (appPlus1b, def)
            -- return (appPlus2, def)

-- runGraph gr sm = runIdentityT
--             . flip SymbolBuilder.evalT sm
--             . flip StarBuilder.evalT Nothing
--             . flip Builder.runT gr
--             . flip NodeBuilder.evalT (Ref $ Node (0 :: Int))

-- literalsTest :: Ref Node -> SymbolMap (Network Label (Maybe Int)) -> Network Label (Maybe Int) -> IO ((), Network Label (Maybe Int))
-- literalsTest i sm gr = runGraph gr sm $ do
--     Literals.assignLiteralTypes i
--     return ()

-- applicationsTest :: Ref Node -> SymbolMap (Network Label (Maybe Int)) -> Network Label (Maybe Int) -> IO ((), Network Label (Maybe Int))
-- applicationsTest i sm gr = runGraph gr sm $ do
--     Applications.assignApplicationTypes i
--     return ()

-- main :: IO ()
-- main = do
--     let ((i, sm), g) = sampleGraph1
--     ((), g') <- literalsTest i sm g
--     -- ((), g') <- applicationsTest i sm g
--     -- pprint g'
--     -- renderAndOpen [ ("g" , g)]
--     renderAndOpen [ ("g" , g')]
--     putStrLn "end"



-- foo :: NetGraph -> IO (Ref $ Node (NetLayers :< Draft Static), NetGraph)
-- foo g = rebuildNetworkM g
--     $ do
--     title "basic element building"
--     s1 <- star
--     s2 <- star
--     print s1

--     title "reading node references"
--     s1_v <- read s1
--     s2_v <- read s2
--     print s1_v

--     title "manual connection builing"
--     c1 <- connection s1 s2

--     title "reading connection references"
--     c1_v <- read c1
--     print c1_v

--     title "edge following"
--     c1_tgt <- follow c1
--     when (c1_tgt /= s2) $ fail "reading is broken!"
--     print "ok!"

--     title "pattern matching"
--     print $ uncover s1_v
--     print $ caseTest (uncover s1_v) $ do
--         match $ \Star -> "its a star! <3"
--         match $ \ANY  -> "something else!"

--     title "complex element building"
--     u1 <- unify s1 s2
--     print u1
--     u1_v <- read u1

--     title "inputs reading"
--     let u1_ins = inputs (uncover u1_v)
--     print u1_ins

--     title "params reading"
--     let s1t = s1_v ^. attr Type
--         s1s = s1_v ^. attr Succs
--         ca1 = case checkAttr Type of
--             Just t  -> show $ s1_v ^. t
--             Nothing -> "no type here!"


sampleGraph2 :: NetGraph -> IO (Ref $ Node (NetLayers :< Draft Static), NetGraph)
sampleGraph2 g = rebuildNetworkM g $ do
    title "basic element building"
    s1 <- string "dupa"
    s2 <- string "jasio"
    i1 <- int 7
    print s1
    return s1

main :: IO ()
main = do
    (star, g) <- prebuild
    print star
    print g
    putStrLn "\n--------------\n"
    (s,g') <- sampleGraph2 g
    print g'

    renderAndOpen [("g", g')]

