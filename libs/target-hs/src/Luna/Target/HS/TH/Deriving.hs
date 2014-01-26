---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}


module Luna.Target.HS.TH.Deriving where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import           Luna.Target.HS.TH.Utils
import qualified Text.Show.Pretty        as PP



deriveShow :: Name -> DecsQ
deriveShow dataName = do
    --dataDec <- getDec dataName
    --let
    --    showName   = mkName "Show"
    --    funcName   = mkName "show"
    --    base       = mkName "base"
    --    dataVars   = map VarT $ getDecVarNames dataDec

    --return $ ppTrace (dataVars) []

    --    inst       = InstanceD [ClassP showName dataVars]
    --                 (AppT (ConT GHC.Show.Show) (foldl AppT (ConT dataName) dataVars))
    --                 [FunD funcName [Clause [VarP base] (NormalB (CaseE (VarE base) [Match (ConP :Interactive.Vector [VarP x_6]) (NormalB (InfixE (Just (LitE (StringL "Vector "))) (VarE GHC.Base.++) (Just (AppE (VarE GHC.Show.show) (VarE x_6))))) []])) []]]]
    return []
