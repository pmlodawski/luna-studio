module Luna.Interpreter.Dirty where

import           Development.Placeholders
import           Luna.Interpreter.Env       (Env)
import qualified Luna.Interpreter.Env       as Env
import           Luna.Interpreter.Label     (Label)
import qualified Luna.Interpreter.Label     as Label
import           Luna.Interpreter.Monad     (InterpreterMonad)
import           Luna.Syntax.Builder        (readRef, writeRef)
import           Luna.Syntax.Builder.Class  (BuilderMonad)
import           Luna.Syntax.Layer.Labeled  (label)
import           Luna.Syntax.Repr.Graph     (Node, Ref)
import           Luna.Syntax.Symbol.Network (Network)
import           Prologue



follow :: InterpreterMonad Env m => Ref Node -> m ()
follow node = do
    Env.addReqNode node
    $notImplemented

markSuccessors :: BuilderMonad (Network Label) m => Ref Node -> m ()
markSuccessors ref = do
    node <- readRef ref
    unless (node ^. label . Label.dirty) $ do
        writeRef ref (node & label . Label.dirty .~ True)
        $notImplemented

    -- unless (node ^. dirty) $ do
    --         node.dirty = True
    --         if node.required || isIO node
    --             reqNodes.add node
    --         mapM markSuccs node.succ
