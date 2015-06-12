---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Module where

import Data.Binary (Binary)

import Flowbox.Prelude
import Luna.Syntax.Decl      (Decl, LDecl, Imp, Decl(Imp))
import Luna.Syntax.Label     (Label)
import Luna.Syntax.Name.Path (QualPath)


data Module a e = Module { _mpath :: QualPath
                         , _body  :: [LDecl a e]
                         } deriving (Show, Generic, Eq, Read)

makeLenses ''Module

type LModule a e = Label a (Module a e)

-- == Utils ==

imports :: Module a e -> [Imp]
imports = mapMaybe isImport . fmap unwrap . view body where
    isImport :: Decl a e -> Maybe Imp
    isImport = \case Imp imp -> Just imp
                     _       -> Nothing


-- == Instances ==

instance (Binary a, Binary e) => Binary (Module a e)

instance (Default a, Default e) => Default (Module a e) where
    def = Module def def


