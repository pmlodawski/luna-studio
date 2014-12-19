---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}


module Luna.Target.HS.Host.Naming2 where

import Flowbox.Prelude

import Language.Haskell.TH



mkFieldAccessor accName typeName conName memName = mkName $ "field" <> accName <> "_"
                                                           <> typeName  <> "_"
                                                           <> conName   <> "_"
                                                           <> memName

mkFieldGetter = mkFieldAccessor "Getter"
mkFieldSetter = mkFieldAccessor "Setter"


--mkMemRef base typeName methodName = mkName $ "prop" <> base <> "_"
--                                           <> typeName <> "_"
--                                           <> methodName

--mkMemSig = mkMemRef "Sig"
--mkMemDef = mkMemRef "Def"


classHasProp = mkName "HasMem"
funcPropSig  = mkName "memSig"

classFunc   = mkName "Func"
funcGetFunc = mkName "getFunc"


mkMemRef base typeName methodName = "mem" <> base       <> "_"
                                          <> typeName   <> "_"
                                          <> methodName

mkMemSig = mkMemRef "Sig"
mkMemDef = mkMemRef "Def"



mkCons = ("cons_" <>)

mkModName = ("Module" <>)

member = "member"

self = "self"

mkCls = ("Cls_" <>)

mkVar    = ("_" <>)