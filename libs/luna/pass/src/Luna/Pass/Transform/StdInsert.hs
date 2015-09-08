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

module Luna.Pass.Transform.StdInsert where


import           Flowbox.Prelude 
import           Luna.Pass              (Pass (Pass), PassCtx, PassMonad)
import qualified Luna.Syntax.Unit       as Unit
import           Luna.Syntax.Unit       (ASTUnit)
import qualified Luna.Syntax.Label      as Label
import           Luna.Syntax.Label      (Label(Label))
import qualified Luna.Syntax.Module     as Module
import           Luna.Syntax.Module     (LModule)
import qualified Luna.Syntax.Decl       as Decl
import           Luna.Syntax.Decl       (LDecl)
import           Luna.Data.ASTInfo      (ASTInfo,  _lastID, genID, incID)
import qualified Luna.Data.ASTInfo      as ASTInfo
import qualified Luna.Syntax.Enum       as Enum
import           Luna.System.Pragma.Store (MonadPragmaStore)
import qualified Luna.System.Pragma.Store  as Pragma
import           Luna.System.Pragma.Store  as Pragma (isEnabled)
import qualified Luna.Parser.Pragma        as Pragma
import           Control.Monad.State

type SAPass m = PassMonad ASTInfo m
type Ctx a m  = (Enum.Enumerated a, MonadPragmaStore m, MonadIO m)

pass :: Ctx a m => Pass ASTInfo (ASTInfo -> ASTUnit a e -> SAPass m (ASTUnit a e, ASTInfo))
pass = Pass "StdLib import insertion"
            "Insertion of standard library"
            undefined iaMain

iaMain :: Ctx a m => ASTInfo -> ASTUnit a e -> SAPass m (ASTUnit a e, ASTInfo)
iaMain astInfo ast = do 
    put astInfo
    newAst <- ifM (isEnabled Pragma.includeStd) (return ast) $ do
        id <- ASTInfo.genID
        return $ (fmap.fmap) (Module.body %~ ((stdImport id):)) ast
    (ast,) <$> get

stdImport :: Enum.Enumerated a => Int -> LDecl a e
stdImport id = Label (Enum.tag id) 
             $ Decl.Imp 
             $ Decl.DeclImp ["FlowboxM", "Libs", "Flowbox", "Std"] 
                            [Decl.Wildcard []]

    
