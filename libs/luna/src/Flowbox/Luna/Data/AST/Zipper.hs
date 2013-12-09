---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

module Flowbox.Luna.Data.AST.Zipper where

import           Flowbox.Control.Monad.Trans.Maybe
import           Flowbox.Luna.Data.AST.Expr        (Expr)
import qualified Flowbox.Luna.Data.AST.Expr        as Expr
import           Flowbox.Luna.Data.AST.Module      (Module)
import qualified Flowbox.Luna.Data.AST.Module      as Module
import           Flowbox.Prelude                   hiding (Zipper, zipper, drop, id, mod)

import Data.List          (find)

--data Focus = FunctionFocus { expr :: Expr   , env :: FocusEnv }
--           | ClassFocus    { expr :: Expr   , env :: FocusEnv }
--           | ModuleFocus   { mod  :: Module , env :: FocusEnv }
--           deriving (Show)

data Focus  = FunctionFocus Expr
            | ModuleFocus Module
            deriving (Show)

type FocusPath = [Focus]

type Zipper = (Focus, FocusPath)

mk :: Module -> Maybe Zipper
mk rootmod = Just (ModuleFocus rootmod, [])

defocus :: Zipper -> Zipper
defocus zipper@(_, [])   = zipper
defocus (env, parent:path) = (newenv, path) where
    newenv = case parent of
        ModuleFocus pmod -> ModuleFocus $ case env of
            FunctionFocus func -> Module.addMethod func pmod


modify :: (Focus -> Focus) -> Zipper -> Maybe Zipper
modify f (env, path) = Just (f env, path)


close :: Zipper -> Maybe Module
close (env, []) = Just mod where ModuleFocus mod = env
close zipper    = close $ defocus zipper


focusFunction :: String -> Zipper -> Maybe Zipper
focusFunction name zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.methods Expr.name
                     FunctionFocus ModuleFocus mod name zipper
    _             -> Nothing


getFocus :: Zipper -> Focus
getFocus = fst

focusListElem :: Lens' a [b] -> Traversal' b String -> (b -> Focus) -> (a -> Focus) -> a -> String -> Zipper -> Maybe Zipper
focusListElem flens nameLens elemFocus crumbFocus el name (_, path) = runMaybe $ do
    let funcs    = el ^. flens
        mfunc    = find (\f -> f ^. nameLens == name) funcs
        newfuncs = [ f | f <- funcs, f ^. nameLens /= name ]
        newelem  = el & flens .~ newfuncs
    func <- hoistMaybe mfunc
    return $ (elemFocus func, (crumbFocus newelem) : path)



