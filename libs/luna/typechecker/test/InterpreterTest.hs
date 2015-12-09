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

import Prologue hiding (Cons, Indexable, Ixed, Repr, Simple, children, cons, empty, index, lookup, maxBound, minBound, repr, simple)

import           Control.Error.Util                   (hush)
import           Control.Monad.Fix
import           Control.Monad.ST
import qualified Control.Monad.State                  as State
import           Control.Monad.State.Generate         (newState)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Data.Cata
import           Data.Constraint
import           Data.Constraint.Void
import           Data.Construction
import           Data.Container
import           Data.Container.Auto
import           Data.Container.Hetero                (Ptr (Ptr), ptrIdx)
import           Data.Container.Hetero
import           Data.Container.Immersed
import qualified Data.Container.Instances.Vector.Lazy as Lazy
import qualified Data.Container.Opts                  as Mods
import           Data.Container.Parametrized
import           Data.Container.Poly
import           Data.Container.Resizable
import           Data.Container.Reusable
import           Data.Container.Weak
import           Data.Convert
import           Data.Convert.Errors                  (TypeMismatch (TypeMismatch))
import           Data.IntMap.Lazy                     (IntMap)
import qualified Data.IntMap.Lazy                     as IntMap
import           Data.IntSet                          (IntSet)
import qualified Data.IntSet                          as IntSet
import           Data.IORef
import           Data.Layer
import           Data.Layer.Coat
import qualified Data.List                            as List
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromJust)
import           Data.Reprx
import           Data.STRef
import qualified Data.Text.AutoBuilder                as Text
import           Data.Text.CodeBuilder.Builder        as CB hiding (app, render)
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as Text
import           Data.Typeable                        hiding (cast)
import           Data.Variants                        hiding (cons)
import qualified Data.Variants                        as V
import           Data.Vector                          (Vector)
import qualified Data.Vector                          as Vector
import           Data.Vector.Dynamic                  as VD
import           Data.Vector.Mutable                  ()
import           Flowbox.System.Types                 hiding (Index, insert, (.:))
import           GHC.Int
import           GHC.Prim                             (Any)
import qualified Language.Haskell.Session             as HS
import           Luna.Diagnostic.AST                  as Diag (display, open, render, toGraphViz)
import qualified Luna.Interpreter.Session             as Session
import           Luna.Syntax.AST
import qualified Luna.Syntax.AST.Arg                  as Arg
import           Luna.Syntax.AST.Decl
import           Luna.Syntax.AST.Lit
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Typed
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Builder                  as Builder
import qualified Luna.Syntax.Builder.Class            as Builder
import           Luna.Syntax.Builder.Node             (MonadNodeBuilder)
import qualified Luna.Syntax.Builder.Node             as NodeBuilder
import           Luna.Syntax.Builder.Star             (MonadStarBuilder)
import           Luna.Syntax.Builder.Star             (StarBuilder, StarBuilderT)
import qualified Luna.Syntax.Builder.Star             as StarBuilder
import qualified Luna.Syntax.Builder.Symbol           as SymbolBuilder
import           Luna.Syntax.Layer.Labeled
import           Luna.Syntax.Name.Pool
import           Luna.Syntax.Repr.Graph
import qualified Luna.Syntax.Repr.Graph               as GraphBuilder
import           Luna.Syntax.Symbol.Map               (SymbolMap)
import qualified Luna.Syntax.Symbol.Map               as Symbol
import           Luna.Syntax.Symbol.Network           (Network)
import qualified Luna.Syntax.Symbol.QualPath          as QualPath
import qualified System.Mem.Weak                      as Mem
import           System.Process
import           Text.Read                            (readMaybe)
import qualified Type.BaseType                        as BT
import           Unsafe.Coerce                        (unsafeCoerce)

-- ====================================

typed a t = StarBuilder.with (const $ Just t) a

renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)
-- ====================================

prettyPrint = putStrLn . ppShow

sampleGraph :: (Ref Node, Network)
sampleGraph = runIdentity
      $ flip StarBuilder.evalT Nothing
      $ flip Builder.runT def
      $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
      $ do
            i2 <- _int 2
            i3 <- _int 3
            namePlus <- _string "+"
            accPlus  <- accessor namePlus i2
            nameInt  <- _string "Int"
            consInt  <- cons nameInt
            arr      <- arrow [consInt, consInt] Map.empty consInt
            appPlus  <- app accPlus [arg i2, arg i3] `typed` arr
            -- x <- app accPlus [arg appPlus, arg appPlus] `typed` int2int2int
            return appPlus


symbolMap :: SymbolMap t
symbolMap = Map.fromList [("+", Symbol.PartiallySpecializedNetwork def def)]

runGraph gr = runIdentityT
            . flip SymbolBuilder.evalT symbolMap
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
            typeNode <- follow (node ^. tp)
            typeStr <- typeString typeNode
            putStrLn typeStr
            acc <- follow a
            (name, s) <- getAcc acc
            -- qual <- typeString s
            -- let sourceQPath = QualPath.mk $ qual <> "." <> name
            -- let args = Symbol.Signature typeNode
            -- runExceptT $ Symbol.getSpecialization sourceQPath args

            args <- mapM (runNode <=< follow . Arg.__arec) (inputs p)
            mangled <- mangleName name typeNode
            fun <- lift $ lift $ lift $ lift $ lift $ Session.findSymbol mangled typeStr -- TODO[PM] przerobic na `... => m (cos)`
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
            src  <- runNode    =<< follow s

            return (name, src)

typeString (ref :: Ref Node) = do
    node <- readRef ref
    case' (uncoat (node :: Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) $ do
        match $ \(Arrow p n r) -> do
            items <- mapM typeString =<< mapM follow (p ++ (Map.elems n) ++ [r])
            return $ intercalate " -> " items
        match $ \(Cons t a) -> do
            intercalate " " <$> mapM (typeString <=< follow) (t:a)
        match $ \(String s) -> do
            return $ toString $ toText s
        match $ \(Int i) -> do
            return $ show i

        match $ \ANY -> do
            prettyPrint $ uncoat node
            undefined

getAccName (ref :: Ref Node) = do
    node <- readRef ref
    case' (uncoat (node :: Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) $
        match $ \(String s) -> return s



evaluateTest :: Ref Node -> Network -> IO ((), Network)
evaluateTest i gr = Session.run $ runGraph gr $ do
    r <- runNode i
    putStrLn "RESULT IS:"
    print (Session.unsafeCast r :: Int)


main :: IO ()
main = do
    let (i, g) = sampleGraph
    -- let (lmap, gs) = addStdLiterals g
    -- let (unis, g2) = pass2 lmap gs
    -- let (_   , g3) = pass3 lmap unis g2

    -- renderAndOpen [ ("g" , g)
    --             --   , ("gs", gs)
    --             --   , ("g2", g2)
    --             --   , ("g3", g3)
    --               ]
    renderAndOpen [ ("g" , g)]

    pprint g
    evaluateTest i g
    putStrLn "end"
