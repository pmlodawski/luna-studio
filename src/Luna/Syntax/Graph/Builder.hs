{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Syntax.Graph.Builder where

import Flowbox.Prelude hiding (cons)
import Data.Variant

import Luna.Syntax.Graph.Builder.Class
import Luna.Syntax.Graph
import Luna.Syntax.Arg
import Luna.Syntax.AST
import Luna.Syntax.Name
import Luna.Syntax.Term
import Luna.Syntax.Lit
import Luna.Syntax.Decl


--- === ArgRef ===

type ArgRef  m l a = Arg (m (GraphRef l a))

--- === Graph builders ===

runGraph :: GraphBuilder g a -> BldrState g -> (a, g)
runGraph  = runIdentity .: runGraphT

mkGraphRef :: GraphRefBuilder el m l a => el -> m (GraphRef l a)
mkGraphRef = fmap GraphRef . mkRef



--- === AST builders ===

-- generic builders

toRawMRef :: ToMRef t m l a => t -> m (Rec (GraphPtr l) a)
toRawMRef = fmap (fromRef . fromGraphRef) . toMRef

mkASTRefWith :: (Constructor variant ast, GraphRefBuilder el m l a) => (ast -> m el) -> variant -> m (GraphRef l a)
mkASTRefWith f a = mkGraphRef =<< f (cons a)

varWith :: (Constructor Var ast, GraphRefBuilder el m l a) => (ast -> m el) -> Name -> m (GraphRef l a)
varWith f = mkASTRefWith f . Var

intWith :: (Constructor Lit ast, GraphRefBuilder el m l a) => (ast -> m el) -> Int -> m (GraphRef l a)
intWith f = mkASTRefWith f . Int

accessorWith :: (GraphConstructor Accessor l a ast, GraphRefBuilder el m l a, ToMRef t m l a)
             => (ast -> m el) -> Name -> t -> m (GraphRef l a)
accessorWith f n r = mkASTRefWith f . Accessor n =<< toRawMRef r

appWith :: (GraphConstructor App l a ast, GraphRefBuilder el m l a, ToMRef t m l a)
        => (ast -> m el) -> t -> [ArgRef m l a] -> m (GraphRef l a)
appWith f base args = do
    baseRef <- toRawMRef base
    argRefs <- mapM readArgRef args
    mkASTRefWith f $ App baseRef argRefs
    where readArgRef (Arg n mref) = Arg n . fromRef . fromGraphRef <$> mref

-- specific builders

var :: (Constructor Var ast, GraphRefBuilder el m l a, ASTGen ast m el) => Name -> m (GraphRef l a)
var = varWith genAST

int :: (Constructor Lit ast, GraphRefBuilder el m l a, ASTGen ast m el) => Int -> m (GraphRef l a)
int = intWith genAST

app :: (GraphConstructor App l a ast, GraphRefBuilder el m l a, ToMRef t m l a, ASTGen ast m el)
    => t -> [ArgRef m l a] -> m (GraphRef l a)
app = appWith genAST

accessor :: (GraphConstructor Accessor l a ast, GraphRefBuilder el m l a, ToMRef t m l a, ASTGen ast m el)
         => Name -> t -> m (GraphRef l a)
accessor = accessorWith genAST

arg :: ToMRef t m l a => t -> Arg (m (GraphRef l a))
arg = Arg Nothing . toMRef


-- operators

(@.) = flip accessor
(@$) = app



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