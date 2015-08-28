{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

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

type GraphStarBuilderT s g m = StarBuilderT (Maybe s) (GraphBuilderT g m)
type GraphStarBuilder  s g   = GraphStarBuilderT s g Identity

runGraph :: GraphStarBuilder s g a -> BldrState g -> (a, g)
runGraph = runIdentity .: runGraphT

runGraphT :: Monad m => GraphStarBuilderT s g m a -> BldrState g -> m (a, g)
runGraphT g gs = flip runBuilderT gs $ flip StarBuilder.evalT Nothing $ g


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


class     Monad m                          => ToMuM' a           m t where toMuM' :: a -> m (Mu t)
instance (Monad m         , t ~ t'       ) => ToMuM'    (Mu t')  m t where toMuM' = return
instance (Monad m, (m ~ n), t ~ t'       ) => ToMuM' (n (Mu t')) m t where toMuM' = id
instance (Monad m, LayeredASTCons Lit m t) => ToMuM' String      m t where toMuM' = string
instance (Monad m, LayeredASTCons Lit m t) => ToMuM' Int         m t where toMuM' = int

--instance (Monad m, (m ~ n)) => ToMuM' String     m t where toMuM' = string
--instance LayeredASTCons Lit m t => ToMuM String m t where toMuM = string

--class     Monad m           => ToMuM a          m t | a -> t where toMuM :: a -> m (Mu t)


-- Utils

mkASTRefWith :: (SpecificCons variant ast, MuBuilder a m t) => (ast -> m (a (Mu t))) -> variant -> m (Mu t)
mkASTRefWith f a = buildMu =<< f (specificCons a)

-- Literals

string :: LayeredASTCons Lit m t => String -> m (Mu t)
string = layeredASTCons . String . fromString

int :: LayeredASTCons Lit m t => Int -> m (Mu t)
int = layeredASTCons . Int

-- Arg

arg :: ToMuM a m t => a -> Arg (m (Mu t))
arg = Arg Nothing . toMuM

-- Terms

var :: forall name m t. (ToMuM' name m t, LayeredASTMuCons Var m t) => name -> m (Mu t)
var n = layeredASTCons . Var =<< (toMuM' n :: m (Mu t))

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
                        StarBuilder.put oldstar
                        return ref

genTopStar :: LayeredStarBuilder m t => m ()
genTopStar = do
    s <- getStar
    StarBuilder.put (Just s)

accessor :: forall name src m t. (ToMuM' name m t, ToMuM' src m t, LayeredASTMuCons Accessor m t) => name -> src -> m (Mu t)
accessor n r = do
    mn <- toMuM' n :: m (Mu t)
    mr <- toMuM' r :: m (Mu t)
    layeredASTCons $ Accessor mn mr

app :: (ToMuM a m t, LayeredASTMuCons App m t) => a -> [MuArg m t] -> m (Mu t)
app base args = layeredASTCons =<< (App <$> toMuM base <*> (sequence . fmap sequence) args)

-- Drafts

blank :: LayeredASTCons Blank m t => m (Mu t)
blank = layeredASTCons Blank


-- operators

(@.) = flip accessor
(@$) = app


-- === Function ===

--evalFunctionBuilderT :: Monad m => GraphStarBuilderT s g m a -> BldrState g -> m a
--execFunctionBuilderT :: Monad m => GraphStarBuilderT s g m a -> BldrState g -> m (Function g)
--runFunctionBuilderT  :: Monad m => GraphStarBuilderT s g m a -> BldrState g -> m (a, Function g)

--evalFunctionBuilderT bldr s = fst <$> runFunctionBuilderT bldr s
--execFunctionBuilderT bldr s = snd <$> runFunctionBuilderT bldr s
--runFunctionBuilderT  bldr s = do
--    (a, g) <- runGraphT bldr s
--    return $ (a, Function g)


--evalFunctionBuilder :: GraphStarBuilder s g a -> BldrState g -> a
--execFunctionBuilder :: GraphStarBuilder s g a -> BldrState g -> Function g
--runFunctionBuilder  :: GraphStarBuilder s g a -> BldrState g -> (a, Function g)

--evalFunctionBuilder = runIdentity .: evalFunctionBuilderT
--execFunctionBuilder = runIdentity .: execFunctionBuilderT
--runFunctionBuilder  = runIdentity .: runFunctionBuilderT
