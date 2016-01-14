module Luna.Interpreter.Interpreter where

import           Development.Placeholders
import qualified Luna.Interpreter.Dirty     as Dirty
import           Luna.Interpreter.Env       (Env)
import qualified Luna.Interpreter.Env       as Env
import           Luna.Interpreter.Label     (Label)
import qualified Luna.Interpreter.Label     as Label
import           Luna.Interpreter.Monad     (InterpreterMonad, get)
import           Luna.Syntax.Builder        (readRef, withRef)
import           Luna.Syntax.Builder.Class  (BuilderMonad)
import           Luna.Syntax.Layer.Labeled  (label)
import           Luna.Syntax.Repr.Graph     (Node, Ref)
import           Luna.Syntax.Symbol.Network (Network)
import           Prologue



execute :: (InterpreterMonad Env m, BuilderMonad (Network Label) m) => m ()
execute = do
    mapM_ Dirty.follow =<< Env.getReqNodes
    mapM_ run =<< topsort =<< Env.getReqNodes
    Env.clearReqNodes

connect :: BuilderMonad (Network Label) m => Ref Node -> Ref Node -> m ()
connect prev next = do
    isPrevDirty <- view (label . Label.dirty) <$> readRef prev
    Dirty.markSuccessors $ if isPrevDirty
        then prev
        else next

markModified :: BuilderMonad (Network Label) m => Ref Node -> m ()
markModified = Dirty.markSuccessors

run :: BuilderMonad (Network Label) m => Ref Node -> m ()
run ref = do
    $(todo "inline node")
    $(todo "compute node")
    withRef ref (label . Label.dirty .~ False)

topsort :: [a] -> m [a]
topsort = $notImplemented



-- def inline node:
--     specialization = SymbolMap.lookup node.type node.name
--     node.spec = specialization
--
-- def compute node:
--     if node.isIO:
--         computeIO node
--     elif node.isReq:
--         for node.reqests as req:
--             reportValue req node
--
-- def computeIO node:
--     v <- runIO =<< compile node.spec
--     node.spec = v
--
-- def reportValue req node:
--     v <- req =<< compile node.spec
--     reportToGUI node.location v
--
-- def compile node: -- # Node -> a
--     ... -- call ghc
--
-- def decompile node:
--     for s in node.succ:
--         s.pre = node
