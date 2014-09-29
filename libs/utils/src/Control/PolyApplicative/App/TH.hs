{-# LANGUAGE TemplateHaskell #-}

module Control.PolyApplicative.App.TH where

import Language.Haskell.TH
import Prelude

mkApp :: Int -> Int -> Q [Dec]
mkApp min max = do
    let range      = [min..max]
        fnames     = fmap (mkName . ("app" ++) . show) range
        cnames     = fmap (mkName . ("dot" ++) . show) range
        fnamesPrev = fmap (mkName . ("app" ++) . show . (flip (-) 1)) range
        app1       = mkName "app1"
        names      = zip3 fnames cnames fnamesPrev

    return $ map (\(name, cname, pname) -> ValD (VarP name) (NormalB (AppE (AppE (VarE cname) (VarE app1)) (VarE pname))) []) names


