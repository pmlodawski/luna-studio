---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.AST.Inspect where

import Control.Monad.State hiding (mapM, mapM_)

import           Flowbox.Control.Error
import           Flowbox.Prelude                        hiding (inside)
import           Flowbox.Source.Location                (loc)
import           Luna.AST.Control.Crumb                 (Breadcrumbs)
import qualified Luna.AST.Control.Crumb                 as Crumb
import           Luna.Interpreter.Session.Data.DefPoint (DefPoint (DefPoint))
import qualified Luna.Interpreter.Session.Env           as Env
import qualified Luna.Interpreter.Session.Error         as Error
import           Luna.Interpreter.Session.Session       (Session)
import qualified Luna.Lib.Lib                           as Library
import qualified Luna.Pass.Analysis.NameResolver        as NameResolver



fromName :: String -> Breadcrumbs -> Library.ID -> Session (Maybe DefPoint)
fromName name parentBC libraryID = do
    libManager <- Env.getLibManager
    results    <- Env.runPass $(loc) $ NameResolver.run name parentBC libraryID libManager
    case results of
        []            -> return Nothing
        [(libID, bc)] -> case last bc of
                            Crumb.Function {} -> return $ Just $ DefPoint libID bc
                            _                 -> return Nothing
        _             -> left $ Error.NameResolverError $(loc) "Name resolver returned multiple results"
