{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Main where

import Prologue hiding (Cons, Indexable, Ixed, Repr, Simple, children, cons, empty, index, lookup, maxBound, minBound, repr, s, simple)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Data.Layer.Coat
import qualified Data.Map                             as Map
import           Data.Variants                        hiding (cons)
import           Data.Vector.Mutable                  ()
import           GHC.Prim                             (Any)
import           Luna.Diagnostic.AST                  as Diag (open, render, toGraphViz)
import qualified Luna.Interpreter.Session             as Session
import qualified Luna.Syntax.AST.Arg                  as Arg
import           Luna.Syntax.AST.Lit
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Typed
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Builder                  as Builder
import qualified Luna.Syntax.Builder.Node             as NodeBuilder
import qualified Luna.Syntax.Builder.Star             as StarBuilder
import qualified Luna.Syntax.Builder.Symbol           as SymbolBuilder
import           Luna.Syntax.Layer.Labeled
import           Luna.Syntax.Repr.Graph
import           Luna.Syntax.Symbol.Map               (SymbolMap)
import qualified Luna.Syntax.Symbol.Map               as Symbol
import           Luna.Syntax.Symbol.Network           (Network)
import qualified Luna.Syntax.Symbol.QualPath          as QualPath

-- ====================================

typed a t = StarBuilder.with (const $ Just t) a

renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)
-- ====================================

prettyPrint = putStrLn . ppShow

sampleGraph :: ((Ref Node, Draft (Ref Edge)), Network)
sampleGraph = runIdentity
      $ flip StarBuilder.evalT Nothing
      $ flip Builder.runT def
      $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
      $ do
            nameInt  <- _string "Int"
            consInt  <- cons nameInt
            i2 <- _int 2 `typed` consInt
            i3 <- _int 3 `typed` consInt
            namePlus <- _string "+"
            accPlus  <- accessor namePlus i2
            arr      <- arrow [consInt, consInt] Map.empty consInt
            appPlus  <- app accPlus [arg i2, arg i3] `typed` arr

            arrNode <- readRef arr
            return (appPlus, uncoat arrNode)


mkSymbolMap :: Arrow t -> SymbolMap t
mkSymbolMap arr = Map.fromList [("Int.+", Symbol.PartiallySpecializedNetwork def $ Map.singleton (Symbol.fromArrow' arr) def )]

runGraph gr sm = runIdentityT
            . flip SymbolBuilder.evalT sm
            . flip StarBuilder.evalT Nothing
            . flip Builder.runT gr
            . flip NodeBuilder.evalT (Ref $ Node (0 :: Int))


mangleName name _ = return (par name)

par x = "(" <> x <> ")"

-- TODO[PM]
-- newtype MyAny = forall (Repr a) => MyAny a -- Prosze zanim to zrobisz sprawdz google -> roznica pomiedzy exystential i Any (tam gdzie jest zadeklarowane Any powinno byc napisane)

-- runNode :: _ => _ -- TODO[PM] Ref Node -> t (t1 (t2 (t3 Control.Monad.Ghc.Ghc))) AnyRef Node -> t (t1 (t2 (t3 Control.Monad.Ghc.Ghc))) Any
runNode (ref :: Ref Node) = do
    node <- readRef ref
    case' (uncoat (node :: Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) $ do
        match $ \(App a p) -> do
            putStrLn "App"
            typeRef <- follow (node ^. tp)
            acc <- follow a
            (name, s) <- getAcc acc
            qual <- typeString s
            let sourceQPath = QualPath.mk $ qual <> "." <> name
            typeNode <- readRef typeRef
            let args' = case' (uncoat typeNode) $ do
                    match $ \a@(Arrow {}) -> Symbol.fromArrow a
            r <- Symbol.toArrow . fst <$> Symbol.getSpecialization sourceQPath args'
            typeStr <- stringArrow r

            args <- mapM (runNode <=< follow . Arg.__arec) (inputs p)
            mangled <- mangleName name typeRef
            fun <- lift $ lift $ lift $ lift $ lift $ lift $ Session.findSymbol mangled typeStr -- TODO[PM] przerobic na `... => m (cos)`
            mapM_ (\r -> print (Session.unsafeCast r :: Int)) args
            return $ foldl Session.appArg fun args
        match $ \(Val v) -> do
            case' v $ do
                match $ \(Int i) -> do
                    return (Session.toAny i :: Any)
                match $ \(String s) -> do
                    return (Session.toAny s :: Any)

        match $ \ANY -> do
            putStrLn "OTHER"
            prettyPrint (uncoat node)
            return undefined

-- TODO[PM] move to runNode
getAcc (ref :: Ref Node) = do
    node <- readRef ref
    case' (uncoat (node :: Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) $ do
        match $ \(Accessor n s) -> do
            name <- typeString =<< follow n
            src  <- follow =<< (view tp <$> (readRef =<< follow s))
            return (name, src)

typeString (ref :: Ref Node) = do
    node <- readRef ref
    case' (uncoat (node :: Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) $ do
        match $ \a@(Arrow {}) -> stringArrow a
        match $ \(Cons t a) -> intercalate " " <$> mapM (typeString <=< follow) (t:a)
        match $ \(String s) -> return $ toString $ toText s
        match $ \(Int i)    -> return $ show i
        match $ \(Val v)    -> do
            case' v $ do
                match $ \(Int i)    -> return $ show i
                match $ \(String s) -> return $ toString $ toText s
        match $ \ANY -> prettyPrint (uncoat node) >> undefined

stringArrow (Arrow p n r) = do
    items <- mapM typeString =<< mapM follow (p ++ (Map.elems n) ++ [r])
    return $ intercalate " -> " items

getAccName (ref :: Ref Node) = do
    node <- readRef ref
    case' (uncoat (node :: Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) $
        match $ \(String s) -> return s



evaluateTest :: Ref Node -> Arrow (Ref Edge) -> Network -> IO ((), Network)
evaluateTest i a gr = Session.run $ runGraph gr (mkSymbolMap a) $ do
    Right r <- runExceptT $ runNode i
    putStrLn "RESULT IS:"
    print (Session.unsafeCast r :: Int)


main :: IO ()
main = do
    let ((i, a), g) = sampleGraph
    -- let (lmap, gs) = addStdLiterals g
    -- let (unis, g2) = pass2 lmap gs
    -- let (_   , g3) = pass3 lmap unis g2

    -- renderAndOpen [ ("g" , g)
    --             --   , ("gs", gs)
    --             --   , ("g2", g2)
    --             --   , ("g3", g3)
    --               ]
    -- renderAndOpen [ ("g" , g)]
    let arr = case' a $ match $ \ar@(Arrow {}) -> ar
    pprint g
    _ <- evaluateTest i arr g
    putStrLn "end"
