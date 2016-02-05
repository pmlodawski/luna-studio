{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Flowbox.GuiMockup.JSInterop where

import           Control.Applicative  ((<$>), (<*>))
import           Control.Lens
import           Data.Binary          (Binary)
import           Data.Foldable
import           Data.List            (intercalate)
import qualified Data.Vector.Storable as V
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics         (Generic)
import           Linear               (V2 (..))



-- FIXME[MM]: this doesn't really belong here, does it?
data CubicBezier a = CubicBezier { cubicC0 :: V2 a
                                 , cubicC1 :: V2 a
                                 , cubicC2 :: V2 a
                                 , cubicC3 :: V2 a
                                 }
    deriving (Eq, Show, Generic, Foldable, Traversable, Functor)

instance Each (CubicBezier a) (CubicBezier b) a b where
    each = traverse

instance (Binary a) => Binary (CubicBezier a)

instance V.Storable a => V.Storable (CubicBezier a) where
    sizeOf _ = 4 * sizeOf (undefined :: V2 a)
    alignment _ = alignment (undefined :: V2 a)
    peek ptr = CubicBezier <$> peek ptr'
                           <*> peekElemOff ptr' 1
                           <*> peekElemOff ptr' 2
                           <*> peekElemOff ptr' 3
        where
            ptr' = castPtr ptr
    poke ptr (CubicBezier c0 c1 c2 c3) = do
        let ptr' = castPtr ptr
        poke ptr' c0
        pokeElemOff ptr' 1 c1
        pokeElemOff ptr' 2 c2
        pokeElemOff ptr' 3 c3

readPoints :: [[Double]] -> V.Vector (V2 Double)
readPoints = V.fromList . map (\[x,y] -> V2 x y)

jsifyVector :: V.Storable a => (a -> String) -> V.Vector a -> String
jsifyVector f = wrap . intercalate "," . map f . V.toList
    where
        wrap s = "[" ++ s ++ "]"

jsifyBezier :: CubicBezier Double -> String
jsifyBezier (CubicBezier c0 c1 c2 c3) = jsifyObject fields points
    where
        points = map jsifyV2 [c0, c1, c2, c3]
        fields = zipWith (:) (repeat 'p') $ map show [(0::Int)..]

jsifyV2 :: V2 Double -> String
jsifyV2 (V2 x y) = "{\"x\": " ++ show x ++ ", \"y\": " ++ show y ++ "}"

jsifyObject :: [String] -> [String] -> String
jsifyObject fields values = wrap $ intercalate "," $ zipWith (\f v -> show f ++ ": " ++ v) fields values
    where
        wrap s = "{" ++ s ++ "}"
