---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

-- FIXME [pm]: [wd] to chyba niepowinien być pass. Funkcje traverse juz mamy. Pogadajmy o tym.

module Luna.Pass.Analysis.ID.Traverse where

import qualified Flowbox.Luna.Data.AST.Common       as AST
import           Flowbox.Luna.Data.AST.Expr         (Expr)
import qualified Flowbox.Luna.Data.AST.Expr         as Expr
import           Flowbox.Luna.Data.AST.Lit          (Lit)
import qualified Flowbox.Luna.Data.AST.Lit          as Lit
import           Flowbox.Luna.Data.AST.Module       (Module)
import qualified Flowbox.Luna.Data.AST.Module       as Module
import           Flowbox.Luna.Data.AST.Pat          (Pat)
import qualified Flowbox.Luna.Data.AST.Pat          as Pat
import           Flowbox.Luna.Data.AST.Type         (Type)
import qualified Flowbox.Luna.Data.AST.Type         as Type
import           Flowbox.Luna.Data.AST.Zipper.Focus (Focus)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus as Focus
import           Flowbox.Prelude                    hiding (mapM, mapM_, op)



traverseFocus :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Focus -> m ()
traverseFocus op f = Focus.traverseM_ (traverseModule op) (traverseExpr op) f


traverseModule :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Module -> m ()
traverseModule op m = do op $ m ^. Module.id
                         Module.traverseM_ (traverseModule op) (traverseExpr op) (traverseType op) (traversePat op) (traverseLit op) m


traverseExpr :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Expr -> m ()
traverseExpr op e = do op $ e ^. Expr.id
                       Expr.traverseM_ (traverseExpr op) (traverseType op) (traversePat op) (traverseLit op) e


traversePat :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Pat -> m ()
traversePat op p = do op $ p ^. Pat.id
                      Pat.traverseM_ (traversePat op) (traverseType op) (traverseLit op) p


traverseType :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Type -> m ()
traverseType op t = do op $ t ^. Type.id
                       Type.traverseM_ (traverseType op) t


traverseLit :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Lit -> m ()
traverseLit op l = op $ l ^. Lit.id
