---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Luna.Target.HS.AST.Builder.TH where

import Data.Monoid
import qualified Luna.Target.HS.AST.Expr as Expr
import qualified Luna.Target.HS.AST.Lit  as Lit

mkMethod tpName funcName = Expr.THE $ Expr.app (Expr.Var "registerMethod")
                         [ Expr.Var $ typeNameRef tpName
                         , Expr.Lit $ Lit.String funcName
                         ]

mkFieldAccessors conName fieldNames = Expr.THE $ Expr.app (Expr.Var "generateFieldAccessors")
                                               [ Expr.Var   $ varNameRef conName
                                               , Expr.ListE $ fmap mconv fieldNames
                                               ]
    where mconv v = case v of
                    Just a  -> Expr.AppE (Expr.VarE "Just") (Expr.Lit $ Lit.String a)
                    Nothing -> Expr.VarE "Nothing"


registerMethod tpName funcName = Expr.THE $ Expr.app (Expr.Var "registerMethod")
                               [ Expr.Var $ typeNameRef tpName
                               , Expr.Lit $ Lit.String funcName
                               ]


typeNameRef = ("''" <>)
varNameRef  = ("'"  <>)



mkFieldAccessors2 typeName descs = Expr.THE $ Expr.app (Expr.Var "generateFieldAccessors")
                                               [ Expr.Var   $ typeNameRef typeName
                                               , Expr.ListE $ fmap mkConDesc descs
                                               ]
    where mconv v = case v of
                    Just a  -> Expr.AppE (Expr.VarE "Just") (Expr.Lit $ Lit.String a)
                    Nothing -> Expr.VarE "Nothing"
          mkConDesc (conName, fieldNames) = Expr.Tuple [Expr.Var $ varNameRef conName, Expr.ListE $ fmap mconv fieldNames]

mkRegFieldAccessors typeName fieldNames = Expr.THE $ Expr.app (Expr.Var "registerFieldAccessors")
                                               [ Expr.Var   $ typeNameRef typeName
                                               , Expr.ListE $ fmap (Expr.Lit . Lit.String) fieldNames
                                               ]


mkRegType typeName = Expr.THE $ Expr.app (Expr.Var "registerType") [Expr.Var $ typeNameRef typeName]
mkRegCons typeName conNames = Expr.THE $ Expr.app (Expr.Var "registerCons") [ Expr.Var $ typeNameRef typeName
                                                                            , Expr.ListE $ fmap (Expr.Lit . Lit.String) conNames
                                                                            ]


