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

module Luna.Interpreter.NodeRunner where

import Prologue hiding (Cons, Indexable, Ixed, Repr, Simple, children, cons, empty, index, lookup, maxBound, minBound, repr, s, simple)

import qualified Control.Monad.Catch        as Catch
import           Control.Monad.Trans.Except
import           Data.Layer.Coat
import qualified Data.Map                   as Map
import           Data.Variants              hiding (cons)
import           Data.Vector.Mutable        ()
import           GHC.Prim                   (Any)
import qualified Language.Haskell.Session   as HS

import           Data.Text.AutoBuilder       (AutoBuilder)
import qualified Luna.Interpreter.Session    as Session
import qualified Luna.Syntax.AST.Arg         as Arg
import           Luna.Syntax.AST.Lit
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Typed
import           Luna.Syntax.Builder
import           Luna.Syntax.Builder.Class   (BuilderMonad)
import           Luna.Syntax.Builder.Symbol  (MonadSymbolBuilder)
import           Luna.Syntax.Layer.Labeled
import           Luna.Syntax.Repr.Graph
import qualified Luna.Syntax.Symbol.Map      as Symbol
import           Luna.Syntax.Symbol.Network  (Network)
import qualified Luna.Syntax.Symbol.QualPath as QualPath



mangleName name _ = return (par name)

par x = "(" <> x <> ")"

-- TODO[PM]
-- newtype MyAny = forall (Repr a) => MyAny a -- Prosze zanim to zrobisz sprawdz google -> roznica pomiedzy exystential i Any (tam gdzie jest zadeklarowane Any powinno byc napisane)

runNode :: (HS.GhcMonad m,
            Catch.MonadMask m,
            MonadSymbolBuilder (Symbol.SymbolMap (Ref Edge)) m,
            BuilderMonad Network m)
        => Ref Node -> ExceptT Symbol.SymbolError m Any
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
            fun <- lift $ Session.findSymbol mangled typeStr -- TODO[PM] przerobic na `... => m (cos)`
            mapM_ (\r -> print (Session.unsafeCast r :: Int)) args
            return $ foldl Session.appArg fun args
        match $ \(Val v) -> do
            case' v $ do
                match $ \(Int i) -> do
                    return (Session.toAny i :: Any)
                match $ \(String s) -> do
                    return (Session.toAny s :: Any)
        match $ \ANY -> do
            putStrLn "unmatched"
            print (uncoat node)
            return undefined

getAcc :: (MonadIO m, BuilderMonad Network m) => Ref Node -> m (String, Ref Node)
getAcc (ref :: Ref Node) = do
    node <- readRef ref
    case' (uncoat (node :: Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) $ do
        match $ \(Accessor n s) -> do
            name <- typeString =<< follow n
            src  <- follow =<< (view tp <$> (readRef =<< follow s))
            return (name, src)

typeString :: (MonadIO m, BuilderMonad Network m) => Ref Node -> m String
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
        match $ \ANY -> print (uncoat node) >> undefined


stringArrow :: (MonadIO m, BuilderMonad Network m) => Arrow (Ref Edge) -> m String
stringArrow (Arrow p n r) = do
    items <- mapM typeString =<< mapM follow (p ++ (Map.elems n) ++ [r])
    return $ intercalate " -> " items

getAccName :: BuilderMonad Network m => Ref Node -> m AutoBuilder
getAccName (ref :: Ref Node) = do
    node <- readRef ref
    case' (uncoat (node :: Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) $
        match $ \(String s) -> return s
