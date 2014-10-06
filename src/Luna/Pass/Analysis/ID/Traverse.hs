---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

-- FIXME [pm]: [wd] to chyba niepowinien być pass. Funkcje traverse juz mamy. Pogadajmy o tym.

module Luna.Pass.Analysis.ID.Traverse where

import           Flowbox.Prelude        hiding (mapM, mapM_, op)
import           Luna.AST.Arg           (Arg)
import qualified Luna.AST.Arg           as Arg
import qualified Luna.AST.Common        as AST
import           Luna.AST.Control.Focus (Focus)
import qualified Luna.AST.Control.Focus as Focus
import           Luna.AST.Expr          (Expr)
import qualified Luna.AST.Expr          as Expr
import           Luna.AST.Lit           (Lit)
import qualified Luna.AST.Lit           as Lit
import           Luna.AST.Module        (Module)
import qualified Luna.AST.Module        as Module
import           Luna.AST.Pat           (Pat)
import qualified Luna.AST.Pat           as Pat
import           Luna.AST.Type          (Type)
import qualified Luna.AST.Type          as Type



traverseFocus :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Focus -> m ()
traverseFocus op f = Focus.traverseM_ (traverseModule op) (traverseExpr op) f


traverseModule :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Module -> m ()
traverseModule op m = do op $ m ^. Module.id
                         Module.traverseM_ (traverseModule op) (traverseExpr op) (traverseType op) (traversePat op) (traverseLit op) (traverseArg op) m


traverseExpr :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Expr -> m ()
traverseExpr op e = do op $ e ^. Expr.id
                       Expr.traverseM_ (traverseExpr op) (traverseType op) (traversePat op) (traverseLit op) (traverseArg op) e


traversePat :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Pat -> m ()
traversePat op p = do op $ p ^. Pat.id
                      Pat.traverseM_ (traversePat op) (traverseType op) (traverseLit op) p


traverseType :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Type -> m ()
traverseType op t = do op $ t ^. Type.id
                       Type.traverseM_ (traverseType op) t


traverseLit :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Lit -> m ()
traverseLit op l = op $ l ^. Lit.id


traverseArg :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Arg Expr -> m ()
traverseArg op a = op $ a ^. Arg.id
