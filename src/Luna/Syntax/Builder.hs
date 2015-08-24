{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Builder where

import Flowbox.Prelude hiding (cons)
import Data.Variants
import Control.Monad.Fix

import           Luna.Syntax.Builder.Star (StarBuilder, StarBuilderT, MonadStarBuilder)
import qualified Luna.Syntax.Builder.Star as StarBuilder
import           Luna.Syntax.Builder.Graph
import           Luna.Syntax.Name
import           Luna.Syntax.AST
import           Luna.Syntax.AST.Arg
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Lit
import           Luna.Syntax.AST.Decl
import           Luna.Syntax.Layer

import Data.Cata

--- === Graph builders ===


runGraph :: GraphBuilderT g (StarBuilder (Maybe s)) a -> BldrState g -> (a, g)
runGraph = runIdentity .: runGraphT

runGraphT :: Monad m => GraphBuilderT g (StarBuilderT (Maybe s) m) a -> BldrState g -> m (a, g)
runGraphT g gs = flip StarBuilder.evalT Nothing $ runBuilderT g gs

--- === Term builders ===

type LayeredStarBuilder m t = (MonadFix m, MonadStarBuilder (Maybe (Mu t)) m, LayeredASTCons Star m t)

type LayeredASTMuCons v m t = LayeredASTCons (v (Mu t)) m t
type LayeredASTConsCtx v a m t = (SpecificCons v (ASTOf a (Mu t)), MuBuilder a m t, LayerGen (Mu t) m a)
data Impossible a

class                                       LayeredASTCons variant m          t where layeredASTCons :: variant -> m (Mu t)
instance LayeredASTConsCtx variant a m t => LayeredASTCons variant m          t where layeredASTCons = mkASTRefWith genLayers
instance                                    LayeredASTCons variant Impossible t where layeredASTCons = error "Impossible"
                                            -- only to allow using LayeredASTCons as prefix without expanding further.

type MuArg m t = Arg (m (Mu t))

mkASTRefWith :: (SpecificCons variant ast, MuBuilder a m t) => (ast -> m (a (Mu t))) -> variant -> m (Mu t)
mkASTRefWith f a = buildMu =<< f (specificCons a)

arg :: ToMuM a m t => a -> Arg (m (Mu t))
arg = Arg Nothing . toMuM

var :: LayeredASTCons Var m t => Name -> m (Mu t)
var = layeredASTCons . Var

star :: LayeredASTCons Star m t => m (Mu t)
star = layeredASTCons Star

getStar :: LayeredStarBuilder m t => m (Mu t)
getStar = do
    s <- StarBuilder.get
    case s of
        Just    ref -> return ref
        Nothing     -> newStar
    where newStar = mdo oldstar <- StarBuilder.get
                        StarBuilder.put (Just ref)
                        ref <- star
                        --StarBuilder.put oldstar
                        return ref

--

accessor :: (ToMuM a m t, LayeredASTMuCons Accessor m t) => Name -> a -> m (Mu t)
accessor n r = layeredASTCons . Accessor n =<< toMuM r

app :: (ToMuM a m t, LayeredASTMuCons App m t) => a -> [MuArg m t] -> m (Mu t)
app base args = layeredASTCons =<< (App <$> toMuM base <*> (sequence . fmap sequence) args)

-- operators

(@.) = flip accessor
(@$) = app


-- === Function ===

--evalFunctionBuilderT :: Monad m => GraphBuilderT g m a -> BldrState g -> m a
--execFunctionBuilderT :: Monad m => GraphBuilderT g m a -> BldrState g -> m (Function g)
--runFunctionBuilderT  :: Monad m => GraphBuilderT g m a -> BldrState g -> m (a, Function g)

--evalFunctionBuilderT bldr s = fst <$> runFunctionBuilderT bldr s
--execFunctionBuilderT bldr s = snd <$> runFunctionBuilderT bldr s
--runFunctionBuilderT  bldr s = do
--    (a, g) <- runGraphT bldr s
--    return $ (a, Function g)


--evalFunctionBuilder :: GraphBuilder g a -> BldrState g -> a
--execFunctionBuilder :: GraphBuilder g a -> BldrState g -> Function g
--runFunctionBuilder  :: GraphBuilder g a -> BldrState g -> (a, Function g)

--evalFunctionBuilder = runIdentity .: evalFunctionBuilderT
--execFunctionBuilder = runIdentity .: execFunctionBuilderT
--runFunctionBuilder  = runIdentity .: runFunctionBuilderT

---- TODO: generalize
--rebuild f = BldrState [] $ f ^. body