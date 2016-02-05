{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}


module Main where

import           Prologue                               hiding (cons, read, Num)

import           Control.Monad.Error                    (MonadError)
import           Data.Layer.Cover
import           Data.Prop
import           Data.Record                            hiding (Layout)

import           Luna.Passes.Diagnostic.GraphViz
import           Luna.Passes.Inference.Literals
import           Luna.Runtime.Model                     (Dynamic, Static)
import           Luna.Syntax.AST.Term                   hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import qualified Luna.Syntax.AST.Term                   as Term
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder      (MonadBuilder)
import           Luna.Syntax.Model.Network.Builder.Self (MonadSelfBuilder)
import           Luna.Syntax.Model.Network.Builder.Term
import           Luna.Syntax.Model.Network.Builder.Type (MonadTypeBuilder)
import           Luna.Syntax.Model.Network.Term

-- import           Luna.Syntax.Model.Graph.Term    (NetGraph, NetNode)
-- import           Luna.Syntax.Model.Layer.Class   (Succs, Type)


-- ====================================

-- typed a t = StarBuilder.with (const $ Just t) a

-- instance LabelAttrs (Labeled2 Label (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) where
--     labelAttrs n = []
--         -- if n ^. label . Label.checked then [GV.color GVC.Magenta]
--         --                               else []

-- ====================================


renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)


-- title s = putStrLn $ "\n" <> "-- " <> s <> " --"

-- data IDT a = IDT a deriving (Show)


-- data MyGraph (t :: * -> *) = MyGraph deriving (Show)

-- type instance Layout (MyGraph t) term rt = t (Term (MyGraph t) term rt)

prebuild :: IO (Ref $ Node (NetLayers :< Draft Static), NetGraph)
prebuild = runNetworkBuilderT def $ star


-- data ImgAttr = ImgAttr deriving (Show)
-- type instance Attr ImgAttr (Cover x) = String

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

proxy :: Proxy (Ref $ Node (NetLayers :< Draft Static))
proxy = Proxy

assignLiteralTypesTest :: (Ref $ Node (NetLayers :< Draft Static))
                       -> NetGraph
                       -> IO ((), NetGraph)
assignLiteralTypesTest ref g = runNetworkBuilderT g $ assignLiteralTypes proxy

sampleGraph2 :: NetGraph -> IO (Ref $ Node (NetLayers :< Draft Static), NetGraph)
sampleGraph2 g = runNetworkBuilderT g $ do
    i1 <- int 2
    i2 <- int 3
    i3 <- int 4
    s1 <- string "abc"
    s2 <- string "def"
    s3 <- string "ghi"

    accPlus1a  <- acc "+" i1
    appPlus1a  <- app accPlus1a [arg i2]

    accPlus1b  <- acc "+" i3
    appPlus1b  <- app accPlus1b [arg appPlus1a]

    accConc1a  <- acc "++" s2
    appConc1a  <- app accConc1a [arg s1]

    accConc1b  <- acc "++" appConc1a
    appConc1b  <- app accConc1b [arg s3]

    accLen     <- acc "len" appConc1b
    appLen     <- app accLen []

    accPlus2   <- acc "+" appPlus1b
    appPlus2   <- app accPlus2 [arg appLen]

    -- print appPlus2
    return i1

main :: IO ()
main = do
    (star, g) <- prebuild
    -- print star
    -- putStrLn "\n--------------\n"
    -- print g
    (s1, g' ) <- sampleGraph2 g
    (s2, g'') <- assignLiteralTypesTest s1 g'
    -- print g'
    renderAndOpen [("g", g'')]
