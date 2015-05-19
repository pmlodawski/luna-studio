---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE OverloadedStrings         #-}

module Luna.Pass.Analysis.InsertStd where


import           Flowbox.Prelude 
import           Luna.Pass              (Pass (Pass), PassCtx, PassMonad)
import qualified Luna.Syntax.Unit       as U
import qualified Luna.Syntax.Label      as L
import qualified Luna.Syntax.Module     as M
import qualified Luna.Syntax.Decl       as D
import           Luna.Data.ASTInfo      (ASTInfo,  _lastID, genID, incID)
import qualified Luna.Data.ASTInfo      as ASTInfo
import qualified Luna.Syntax.Enum       as Enum
import           Luna.System.Pragma.Store (MonadPragmaStore)
import qualified Luna.System.Pragma.Store  as Pragma
import qualified Luna.System.Pragma        as Pragma (isEnabled)
import qualified Luna.Parser.Pragma        as Pragma

type SAPass m = PassMonad NoState m
data NoState  = NoState deriving Show


pass :: (Enum.Enumerated a, MonadPragmaStore m, MonadIO m) => Pass NoState (ASTInfo -> U.Unit (L.Label l (M.Module a e)) -> SAPass m (U.Unit (L.Label l (M.Module a e)), ASTInfo))
pass = Pass "StdLib import insertion"
            "Insertion of standard library"
            undefined iaMain


iaMain :: (Enum.Enumerated a, MonadPragmaStore m, MonadIO m) => ASTInfo -> U.Unit (L.Label l (M.Module a e)) -> SAPass m (U.Unit (L.Label l (M.Module a e)), ASTInfo)
iaMain astinfo ast = do includeStd <- Pragma.lookup Pragma.includeStd
                        --putStrLn . show $ includeStd
                        case fmap Pragma.isEnabled includeStd of
                            Right True -> return ((U.fromUnit . L.element . M.body) `over`  ((stdImport astinfo):) $ ast, incID astinfo)
                            _          -> return (ast, astinfo)


stdImport :: Enum.Enumerated a => ASTInfo -> D.LDecl a e
stdImport astinfo = L.Label (Enum.tag $ astinfo ^. ASTInfo.lastID) (D.Imp $ D.DeclImp [fromText "FlowboxM", fromText "Libs", fromText "Flowbox", fromText "Std"] [D.Wildcard []])

