{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Graph.Builder where

import Flowbox.Prelude hiding (cons)
import Data.Variants
import Control.Monad.Fix

import Luna.Syntax.Graph.Builder.State
import Luna.Syntax.Graph
import Luna.Syntax.Arg
import Luna.Syntax.AST
import Luna.Syntax.Name
import Luna.Syntax.Term
import Luna.Syntax.Lit
import Luna.Syntax.Decl



tst3 :: (MonadFix m, MuRefBuilder m ref a, ASTGen2 v m a (MuRef ref a)) => v (MuRef ref a) -> m (MuRef ref a)
tst3 a = mdo
    ast <- genAST2 a t
    t   <- mkMuRef ast
    return t

vx :: (MonadFix m, Variant Var (v (MuRef ref a)), MuRefBuilder m ref a, ASTGen2 v m a (MuRef ref a)) => Name -> m (MuRef ref a)
vx = tst3 . cons . Var

--type GraphRefBuilder2 a t m = RefBuilder2 a t m Ref'

--- === ArgRef ===

type ArgRef  m l a = Arg (m (GraphRef l a))

--- === Graph builders ===

runGraph :: GraphBuilder g a -> BldrState g -> (a, g)
runGraph  = runIdentity .: runGraphT

--mkGraphRef :: GraphRefBuilder el m l a => el -> m (GraphRef l a)
--mkGraphRef = fmap GraphRef . mkRef

--mkGraphRef2 :: GraphRefBuilder2 a t m => a t -> m (Ref' a t)
--mkGraphRef2 = mkRef2

--- === Term builders ===

-- generic builders

toRawMRef :: ToMRef t m l a => t -> m (Recx (GraphPtr l) a)
toRawMRef = fmap (fromRef . fromGraphRef) . toMRef

--mkASTRefWith :: (Variant variant ast, GraphRefBuilder el m l a) => (ast -> m el) -> variant -> m (GraphRef l a)
--mkASTRefWith f a = mkGraphRef =<< f (cons a)


--mkASTRefWith2 :: (Variant variant ast, GraphRefBuilder2 a t m) => (ast -> m (a t)) -> variant -> m (Ref' a t)
--mkASTRefWith2 f a = mkGraphRef2 =<< f (cons a)

mkASTRefWith3 :: (Variant variant ast, MuRefBuilder m ref a) => (ast -> m (MuRefData ref a)) -> variant -> m (MuRef ref a)
mkASTRefWith3 f a = mkMuRef =<< f (cons a)

--type LayeredASTCons v m ref a = (MuRefBuilder m ref a, Variant v (MuRefData ref a), LayerGen a (MuRef ref a) m a)

--layeredASTCons :: LayeredASTCons variant m ref a => variant -> m (MuRef ref a)
--layeredASTCons :: (Variant variant (ast (MuRef ref a)), MuRefBuilder m ref a) => ((ast (MuRef ref a)) -> m (MuRefData ref a)) -> variant -> m (MuRef ref a)
--layeredASTCons :: (Variant variant (ast (MuRef ref a)), MuRefBuilder m ref a, LayerGen ast (MuRef ref a) m a) => variant -> m (MuRef ref a)

type LayeredASTGen m ref a = LayerGen  (MuRef    ref a) m a
type MuRefAST        ref a = ASTOf   a (MuRef    ref a)
type MuRefVariant  v ref a = Variant v (MuRefAST ref a)

type LayeredASTCons variant m ref a = (MuRefVariant variant ref a, MuRefBuilder m ref a, LayeredASTGen m ref a)

layeredASTCons :: LayeredASTCons variant m ref a => variant -> m (MuRef ref a)
layeredASTCons = mkASTRefWith3 genLayers

--class LayeredASTCons v m ref a where
--    layeredASTCons :: (Variant variant (ast (MuRef ref a)), MuRefBuilder m ref a, LayerGen ast (MuRef ref a) m a) => variant -> m (MuRef ref a)

--instance LayeredASTCons v m ref a where layeredASTCons = mkASTRefWith3 genLayers

--foo :: LayeredASTCons variant m ref a => variant -> m (MuRef ref a)
--foo = layeredASTCons

--layeredASTCons2 :: _ => _
--layeredASTCons2 = mkASTRefWith3 genLayers

--class Monad m => LayerGen t m l where
--    genLayers :: ASTOf l t -> m (l t)


varx' :: LayeredASTCons Var m ref a => Name -> m (MuRef ref a)
varx' = layeredASTCons . Var

star' :: LayeredASTCons Star m ref a => m (MuRef ref a)
star' = layeredASTCons Star

mkASTRefWith3x :: (Variant variant ast, MuRefBuilder m ref a) => ((MuRefData ref a -> m (MuRef ref a)) -> ast -> m (MuRef ref a)) -> variant -> m (MuRef ref a)
mkASTRefWith3x f a = f mkMuRef (cons a)

--varWith :: (Variant Var ast, GraphRefBuilder el m l a) => (ast -> m el) -> Name -> m (GraphRef l a)
--varWith f = mkASTRefWith f . Var

--starWith :: (Variant Star ast, GraphRefBuilder el m l a) => (ast -> m el) -> m (GraphRef l a)
--starWith f = mkASTRefWith f Star

--varWith2 :: (Variant Var ast, RefBuilder2 a t m Ref') => (ast -> m (a t)) -> Name -> m (Ref' a t)
--varWith2 f = mkASTRefWith2 f . Var

varWith3 :: (Variant Var ast, MuRefBuilder m ref a) => (ast -> m (MuRefData ref a)) -> Name -> m (MuRef ref a)
varWith3 f = mkASTRefWith3 f . Var

varWith3x f = mkASTRefWith3x f . Var

--intWith :: (Variant Lit ast, GraphRefBuilder el m l a) => (ast -> m el) -> Int -> m (GraphRef l a)
--intWith f = mkASTRefWith f . Int

--accessorWith :: (GraphConstructor Accessor l a ast, GraphRefBuilder el m l a, ToMRef t m l a)
--             => (ast -> m el) -> Name -> t -> m (GraphRef l a)
--accessorWith f n r = mkASTRefWith f . Accessor n =<< toRawMRef r

--appWith :: (GraphConstructor App l a ast, GraphRefBuilder el m l a, ToMRef t m l a)
--        => (ast -> m el) -> t -> [ArgRef m l a] -> m (GraphRef l a)
--appWith f base args = do
--    baseRef <- toRawMRef base
--    argRefs <- mapM readArgRef args
--    mkASTRefWith f $ App baseRef argRefs
--    where readArgRef (Arg n mref) = Arg n . fromRef . fromGraphRef <$> mref

-- specific builders

--var :: (Variant Var ast, GraphRefBuilder el m l a, ASTGen ast m el) => Name -> m (GraphRef l a)
--var = varWith genAST

--var2 :: (Variant Var ast, RefBuilder2 a t m Ref', ASTGen ast m (a t)) => Name -> m (Ref' a t)
--var2 = varWith2 genAST
--class CataConstructor v a where
--    cataCons :: v -> a t

--instance Variant v (a t) => CataConstructor v a where
--    cataCons = cons


--dokonczyc, powinno byc forall r!
--type LayeredConstructor2 v r m t a = ( Variant v (r t)
--                                    , LayerGen    r t m a
--                                    )

--type LayeredASTConstructor2 v ast m ref a = ( LayeredConstructor2 v ast m (MuRef ref a) a
--                                           , MuRefBuilder       ref m a
--                                           )

----type MuRefLayerGen a ref m var =
----var3 :: (Variant Var ast, MuRefBuilder ref m var, ASTGen ast m (MuRefData ref var)) => Name -> m (MuRef ref var)
--var3 :: LayeredASTConstructor2 Var ast m ref a => Name -> m (MuRef ref a)
--var3 = varWith3 genLayers

--class HasAST el ast => ASTGen ast m el where
--    genAST :: ast -> m el


--class Monad m => LayerGen a t m l where
--    genLayers :: a t -> m (l t)



--int :: (Variant Lit ast, GraphRefBuilder el m l a, ASTGen ast m el) => Int -> m (GraphRef l a)
--int = intWith genAST

--app :: (GraphConstructor App l a ast, GraphRefBuilder el m l a, ToMRef t m l a, ASTGen ast m el)
--    => t -> [ArgRef m l a] -> m (GraphRef l a)
--app = appWith genAST

--accessor :: (GraphConstructor Accessor l a ast, GraphRefBuilder el m l a, ToMRef t m l a, ASTGen ast m el)
--         => Name -> t -> m (GraphRef l a)
--accessor = accessorWith genAST

arg :: ToMRef t m l a => t -> Arg (m (GraphRef l a))
arg = Arg Nothing . toMRef


-- operators

--(@.) = flip accessor
--(@$) = app



-- === Function ===

evalFunctionBuilderT :: Monad m => GraphBuilderT g m a -> BldrState g -> m a
execFunctionBuilderT :: Monad m => GraphBuilderT g m a -> BldrState g -> m (Function g)
runFunctionBuilderT  :: Monad m => GraphBuilderT g m a -> BldrState g -> m (a, Function g)

evalFunctionBuilderT bldr s = fst <$> runFunctionBuilderT bldr s
execFunctionBuilderT bldr s = snd <$> runFunctionBuilderT bldr s
runFunctionBuilderT  bldr s = do
    (a, g) <- runGraphT bldr s
    return $ (a, Function g)


evalFunctionBuilder :: GraphBuilder g a -> BldrState g -> a
execFunctionBuilder :: GraphBuilder g a -> BldrState g -> Function g
runFunctionBuilder  :: GraphBuilder g a -> BldrState g -> (a, Function g)

evalFunctionBuilder = runIdentity .: evalFunctionBuilderT
execFunctionBuilder = runIdentity .: execFunctionBuilderT
runFunctionBuilder  = runIdentity .: runFunctionBuilderT

-- TODO: generalize
rebuild f = BldrState [] $ f ^. body