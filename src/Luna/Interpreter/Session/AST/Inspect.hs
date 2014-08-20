---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.AST.Inspect where

import Control.Monad.State hiding (mapM, mapM_)

import           Flowbox.Control.Error
import           Flowbox.Prelude                        hiding (inside)
import           Luna.AST.Control.Crumb                 (Breadcrumbs)
import qualified Luna.AST.Control.Crumb                 as Crumb
import           Luna.Interpreter.Session.Data.DefPoint (DefPoint (DefPoint))
import           Luna.Interpreter.Session.Session       (Session)
import qualified Luna.Interpreter.Session.Session       as Session
import qualified Luna.Lib.Lib                           as Library
import qualified Luna.Pass.Analysis.NameResolver        as NameResolver



fromName :: String -> Breadcrumbs -> Library.ID -> Session (Maybe DefPoint)
fromName name parentBC libraryID = do
    libManager <- Session.getLibManager
    results <- EitherT $ NameResolver.run name parentBC libraryID libManager
    case results of
        []            -> return Nothing
        [(libID, bc)] -> case last bc of
                            Crumb.Function {} -> return $ Just $ DefPoint libID bc
                            _                 -> return Nothing
        _             -> left "Name resolver returned multiple results"
