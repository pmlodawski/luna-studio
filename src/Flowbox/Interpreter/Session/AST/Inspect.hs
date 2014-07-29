---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.AST.Inspect where

import Control.Monad.State hiding (mapM, mapM_)

import           Flowbox.Control.Error
import           Flowbox.Interpreter.Session.Data.DefPoint (DefPoint (DefPoint))
import           Flowbox.Interpreter.Session.Session       (Session)
import qualified Flowbox.Interpreter.Session.Session       as Session
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs   (Breadcrumbs)
import qualified Flowbox.Luna.Lib.Library                  as Library
import qualified Flowbox.Luna.Passes.Analysis.NameResolver as NameResolver
import           Flowbox.Prelude                           hiding (inside)



fromName :: String -> Breadcrumbs -> Library.ID -> Session (Maybe DefPoint)
fromName name parentBC libraryID = do
    libManager <- Session.getLibManager
    results <- EitherT $ NameResolver.run name parentBC libraryID libManager
    case results of
        []       -> return   Nothing
        [result] -> return $ Just $ uncurry DefPoint result
        _        -> left "Name resolver returned multiple results"
