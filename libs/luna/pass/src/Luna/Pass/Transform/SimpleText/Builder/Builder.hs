---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Pass.Transform.SimpleText.Builder.Builder where

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Luna.AST.Expr             (Expr)
import           Luna.Graph.PropertyMap    (PropertyMap)
import           Luna.Pass.Pass            (Pass)
import qualified Luna.Pass.Pass            as Pass
import           Luna.Util.LunaShow        (lunaShow)



logger :: Logger
logger = getLogger $(moduleName)


type STBPass result = Pass Pass.NoState result


run :: PropertyMap -> Expr -> Pass.Result (String, PropertyMap)
run = (Pass.run_ (Pass.Info "SimpleTextBuilder") Pass.NoState) .: fun2text


fun2text :: PropertyMap -> Expr -> STBPass (String, PropertyMap)
fun2text propertyMap expr = 
    return (lunaShow expr, propertyMap)
