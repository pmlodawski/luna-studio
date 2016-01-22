module Luna.Interpreter.Dirty where

import           Control.Monad            (forM_)
import qualified Data.IntSet              as IntSet
import           Data.Layer.Coat          (uncoat)
import           Development.Placeholders
import           Prologue                 hiding (pre, succ)

import           Luna.Interpreter.Env       (Env)
import qualified Luna.Interpreter.Env       as Env
import           Luna.Interpreter.Label     (Label)
import qualified Luna.Interpreter.Label     as Label
import           Luna.Interpreter.Monad     (InterpreterMonad)
import           Luna.Syntax.AST.Term
import qualified Luna.Syntax.Builder        as B
import           Luna.Syntax.Builder.Class  (BuilderMonad)
import           Luna.Syntax.Layer.Labeled  (label)
import           Luna.Syntax.Repr.Graph     (Edge (Edge), Node, Ref (Ref))
import qualified Luna.Syntax.Repr.Graph     as G
import           Luna.Syntax.Symbol.Network (Network)
import qualified Luna.Syntax.Layer.Labeled as Labeled
import qualified Luna.Syntax.AST.Typed as Typed



pre :: BuilderMonad (Network Label) m => Ref Node -> m [Ref Node]
pre ref = do
    node <- B.readRef ref
    mapM (B.follow) $ inputs $ uncoat node

succ :: BuilderMonad (Network Label) m => Ref Node -> m [Ref Node]
succ ref = do
    node <- B.readRef ref
    mapM (B.unfollow . Ref . Edge) $ IntSet.toList $ node ^. G.succs

up :: BuilderMonad (Network Label) m => Ref Node -> m (Ref Node)
up ref = do
    node <- B.readRef ref
    B.follow $ node ^. Typed.tp


isDirty :: Labeled.HasLabel Label s => s -> Bool
isDirty node = node ^. label . Label.dirty

isRequired :: Labeled.HasLabel Label s => s -> Bool
isRequired node = node ^. label . Label.required

follow :: (InterpreterMonad Env m,  BuilderMonad (Network Label) m) => Ref Node -> m ()
follow node = do
    Env.addReqNode node
    prevs <- pre node
    forM_ prevs $ \ p ->
        whenM (isDirty <$> B.readRef p) $
            follow p

markSuccessors :: (InterpreterMonad Env m, BuilderMonad (Network Label) m) => Ref Node -> m ()
markSuccessors ref = do
    node <- B.readRef ref
    unless (isDirty node) $ do
        B.writeRef ref (node & label . Label.dirty .~ True)
        when (isRequired node) $ do
            Env.addReqNode ref
        mapM_ markSuccessors =<< succ ref
        markSuccessors =<< up ref
