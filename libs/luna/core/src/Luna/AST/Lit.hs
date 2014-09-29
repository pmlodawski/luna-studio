---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Luna.AST.Lit where

import           Control.Lens
import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Prelude                 (Eq, Read, Show, (++))
import qualified Flowbox.Prelude                 as Prelude
import           GHC.Generics
import           Luna.AST.Common                 (ID)



data Lit = Char    { _id :: ID, _char :: Prelude.Char   }
         | String  { _id :: ID, _str  :: Prelude.String }
         | Integer { _id :: ID, _str  :: Prelude.String }
         | Float   { _id :: ID, _str  :: Prelude.String }
         deriving (Show, Eq, Generic, Read)

instance QShow Lit
makeLenses (''Lit)


lunaShow :: Lit -> Prelude.String
lunaShow lit = case lit of
    Char    _ char' -> '\'' : char' : "'"
    String  _ str'  -> '\"' : str' ++ "\""
    Integer _ str'  -> str'
    Float   _ str'  -> str'
