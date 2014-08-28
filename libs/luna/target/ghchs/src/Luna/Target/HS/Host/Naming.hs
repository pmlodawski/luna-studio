---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.Target.HS.Host.Naming where

import Language.Haskell.TH


class NameCls a where
    withName :: (String -> String) -> (a -> a)
    toStr    :: a -> String
    toName   :: a -> Name

instance NameCls String where
    withName f = f
    toStr      = id
    toName     = mkName

instance NameCls Name where
    withName f = mkName.f.nameBase
    toStr      = nameBase
    toName     = id

mkFieldAccessor accName typeName conName memName = mkName $ "field" ++ accName ++ "_"
                                                           ++ toStr typeName  ++ "_"
                                                           ++ toStr conName   ++ "_"
                                                           ++ toStr memName

mkFieldGetter = mkFieldAccessor "Getter"
mkFieldSetter = mkFieldAccessor "Setter"


--mkMemRef base typeName methodName = mkName $ "prop" ++ base ++ "_"
--                                           ++ toStr typeName ++ "_"
--                                           ++ toStr methodName

--mkMemSig = mkMemRef "Sig"
--mkMemDef = mkMemRef "Def"


classHasProp = mkName "HasMem"
funcPropSig  = mkName "memSig"

classFunc   = mkName "Func"
funcGetFunc = mkName "getFunc"


mkMemRef base typeName methodName = "mem" ++ base ++ "_"
                                          ++ toStr typeName ++ "_"
                                          ++ toStr methodName

mkMemSig = mkMemRef "Sig"
mkMemDef = mkMemRef "Def"


modCon = con -- . mkModName

con = ("cons_" ++)

mkModName = ("Module" ++)
