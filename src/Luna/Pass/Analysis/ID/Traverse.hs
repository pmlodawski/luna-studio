---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

-- FIXME [pm]: [wd] to chyba niepowinien być pass. Funkcje traverse juz mamy. Pogadajmy o tym.

module Luna.Pass.Analysis.ID.Traverse where

import           Flowbox.Prelude           hiding (mapM, mapM_, op)
import           Luna.Syntax.Arg           (Arg)
import           Luna.Syntax.Arg           (Arg)
import qualified Luna.Syntax.Arg           as Arg
import qualified Luna.Syntax.Arg           as Arg
import qualified Luna.Syntax.AST           as AST
import           Luna.Syntax.Control.Focus (Focus)
import qualified Luna.Syntax.Control.Focus as Focus
import           Luna.Syntax.Expr          (Expr)
import qualified Luna.Syntax.Expr          as Expr
import           Luna.Syntax.Lit           (Lit)
import qualified Luna.Syntax.Lit           as Lit
import           Luna.Syntax.Module        (Module)
import qualified Luna.Syntax.Module        as Module
import           Luna.Syntax.Pat           (Pat)
import qualified Luna.Syntax.Pat           as Pat
import           Luna.Syntax.Type          (Type)
import qualified Luna.Syntax.Type          as Type



--traverseFocus :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Focus -> m ()
--traverseFocus op = void . Focus.traverseMR (processModule op) (processExpr op)
--     (processType op) (processPat op) (processLit op) (processArg op)


--traverseModule :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Module -> m ()
--traverseModule op = void . Module.traverseMR (processModule op) (processExpr op)
--    (processType op) (processPat op) (processLit op) (processArg op)


--traverseExpr :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Expr -> m ()
--traverseExpr op = void . Expr.traverseMR (processExpr op) (processType op)
--    (processPat op) (processLit op) (processArg op)


--traversePat :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Pat -> m ()
--traversePat op = void . Pat.traverseMR (processPat op) (processType op) (processLit op)


--traverseType :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Type -> m ()
--traverseType op = void . Type.traverseMR (processType op)


--traverseLit :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Lit -> m ()
--traverseLit op l = op $ l ^. Lit.id


--processModule :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Module -> m Module
--processModule op m = op (m ^. Module.id) >> return m


--processExpr :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Expr -> m Expr
--processExpr op e = op (e ^. Expr.id) >> return e


--processPat :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Pat -> m Pat
--processPat op p = op (p ^. Pat.id) >> return p


--processType :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Type -> m Type
--processType op t = op (t ^. Type.id) >> return t


--processLit :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Lit -> m Lit
--processLit op l = op (l ^. Lit.id) >> return l


--processArg :: (Applicative m, Monad m) => (AST.ID -> m ()) -> Arg a -> m (Arg a)
--processArg op a = op (a ^. Arg.id) >> return a
