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

import Prologue

import           Control.Monad.Catch         (MonadMask, catchAll)
import           Control.Monad               (forM)
import           Data.Layer.Coat             (uncoat, Coat, Uncoated, Coated)
import qualified Data.Text.Lazy              as Text
import           Language.Haskell.Session    (GhcMonad)
import           GHC.Prim                    (Any)
import           Data.Variants               (match, case', ANY(..))
import           Data.Layer                  (Unlayered)
import           Data.Maybe                  (fromMaybe, catMaybes)
import qualified Data.Map                    as Map
import           Data.Map                    (Map)

import           Luna.Syntax.Builder.Class   (BuilderMonad)
import qualified Luna.Syntax.Builder         as Builder
import           Luna.Syntax.Repr.Graph      (Graph)

import qualified Luna.Syntax.Builder         as Builder
import qualified Luna.Syntax.Builder.Node    as NodeBuilder
import qualified Luna.Syntax.Builder.Star    as StarBuilder
import           Luna.Syntax.Repr.Graph      (Ref(..), Node(..), Edge, DoubleArc)
import           Luna.Syntax.AST.Term        (Var(..), App(..), Blank(..), Accessor(..), Unify(..), Val(..), Draft)
import qualified Luna.Syntax.AST.Arg         as Arg
import qualified Luna.Syntax.AST.Lit         as Lit

import qualified Luna.Interpreter.Session    as Session

import qualified Luna.StdLib.Int             as StdLibInt

type NodeType a       = (Coated a, Uncoated a ~ (Draft (Ref Edge)))
type GraphBuilder m a = (Coated a, Uncoated a ~ (Draft (Ref Edge)), BuilderMonad (Graph a DoubleArc) m)

type Bindings         = Map (Ref Node) (Ref Node)

runInterpreterM gr = fmap fst
                   . Session.run
                   . flip StarBuilder.evalT Nothing
                   . flip Builder.runT gr
                   . flip NodeBuilder.evalT (Ref $ Node (0 :: Int))

getNodeValues :: NodeType a => [Ref Node] -> Graph a DoubleArc -> IO (Map (Ref Node) Int)
getNodeValues refs gr = runInterpreterM gr $ do
    bindings <- prepareBindings refs
    fmap (Map.fromList . catMaybes) $ forM refs $ \ref -> do
        val <- flip catchAll (\e -> print e >> return Nothing) $ evalNode bindings ref
        case val of
            Nothing -> return Nothing
            Just v  -> return $ Just (ref, Session.unsafeCast v)

prepareBindings :: GraphBuilder m a => [Ref Node] -> m Bindings
prepareBindings refs = fmap Map.fromList $ forM refs $ \ref -> do
    node <- Builder.readRef ref
    case' (uncoat node) $
        match $ \(Unify var tgt) -> (,) <$> Builder.follow var <*> Builder.follow tgt

printIdent :: GraphBuilder m a => Ref Node -> m String
printIdent ref = do
    node <- Builder.readRef ref
    case' (uncoat node) $ match $ \(Lit.String n) -> return (Text.unpack . toText $ n)

destructFunNode :: GraphBuilder m a => Ref Node -> m (String, [Ref Node])
destructFunNode ref = do
    node <- Builder.readRef ref
    case' (uncoat node) $ do
        match $ \(Var n) -> do
            name <- Builder.follow n >>= printIdent
            return (name, [])
        match $ \(Accessor n t) -> do
            name <- Builder.follow n >>= printIdent
            arg  <- Builder.follow t
            return (name, [arg])

mangleName :: String -> String
mangleName name = "(" <> name <> ")"

getVal :: Val a -> Any
getVal val = case' val $ do
    match $ \(Lit.Int i) -> Session.toAny i

evalNode :: (GhcMonad m, MonadMask m, GraphBuilder m a) => Bindings -> Ref Node -> m (Maybe Any)
evalNode bindings ref = runNode bindings $ fromMaybe ref $ Map.lookup ref bindings

runNode :: (GhcMonad m, MonadMask m, GraphBuilder m a) => Bindings -> Ref Node -> m (Maybe Any)
runNode bindings ref = do
    node <- Builder.readRef ref
    case' (uncoat node) $ do
        match $ \(Unify l r) -> Builder.follow r >>= runNode bindings
        match $ \(App f args) -> do
            (funName, initArgs) <- Builder.follow f >>= destructFunNode
            tailArgs <- mapM (Builder.follow . Arg.__arec) args
            argsMay <- fmap sequence $ mapM (evalNode bindings) $ initArgs ++ tailArgs
            case argsMay of
                Nothing   -> return (Nothing :: Maybe Any)
                Just args -> do
                      let tpe = intercalate " -> " $ replicate (1 + length args) "Int"
                      fun <- Session.findSymbol (mangleName funName) tpe
                      return . Just $ foldl Session.appArg fun args
        match $ return . Just . getVal
        match $ \ANY -> return (Nothing :: Maybe Any)
