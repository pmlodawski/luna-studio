{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Container.TH where

import Prologue
import Language.Haskell.TH

--QueryResultM MeasurableInfo q t m Int

--mkQM name fname = return [ClassD [appsT (ConT instCtx) params] qmN args [] [SigD fqmN (arrow (AppT (ConT proxy) $ VarT qN) (arrow (VarT tN) (VarT mN)))   ]] where
mkQM name fname sig func = do
    SigE _ sigT <- sig
    let sig'    = withSigTail (AppT $ appsT (ConT resultM) (ConT infoN : params)) $ getRawSig sigT
    (sig >>= (runIO . print)) *> return [ClassD [appsT (ConT instCtx) params] qmN args [] [SigD fqmN (arrows (AppT (ConT proxy) $ VarT qN) [VarT tN, sig'] )   ]] where
        instCtx = mkName "CTXOO"
        fqmN    = mkName $ fname ++ "QM"
        qmN     = mkName $ name ++ "QM"
        infoN   = mkName $ name ++ "Info"
        qN      = mkName "q"
        mN      = mkName "m"
        tN      = mkName "t"
        showN   = mkName "Show"
        caN     = mkName "A"
        paN     = mkName "a"
        proxy   = mkName "Proxy"
        params  = VarT    <$> [qN, mN, tN]
        args    = PlainTV <$> [qN, mN, tN]
        resultM = mkName "QueryResultM"


getRawSig :: Type -> Type
getRawSig = \case ForallT _ _ s -> s
                  t             -> t

withSigTail f = \case
    AppT a b -> AppT a $ withSigTail f b
    t        -> f t

appsT = foldl AppT

arrow = AppT . AppT ArrowT
--arrows = foldr $ flip arrow


arrows base args = arrows' (base : args) where
    arrows' args = case args of (a:[]) -> a
                                (a:as) -> arrow a (arrows' as)