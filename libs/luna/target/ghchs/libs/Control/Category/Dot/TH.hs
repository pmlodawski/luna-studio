{-# LANGUAGE TemplateHaskell #-}

module Control.Category.Dot.TH where

import Language.Haskell.TH

mkDots :: Int -> Int -> Q [Dec]
mkDots min max = do
    let range      = [min..max]
        fnames     = fmap (mkName . ("dot" ++) . show) range
        fnamesPrev = fmap (mkName . ("dot" ++) . show . (flip (-) 1)) range
        dot1       = mkName "dot1"
        names      = zip fnames fnamesPrev

    return $ map (\(name, pname) -> ValD (VarP name) (NormalB (AppE (AppE (VarE dot1) (VarE pname)) (VarE dot1))) []) names


