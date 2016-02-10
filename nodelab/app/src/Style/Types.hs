module Style.Types where

import Utils.PreludePlus

type Color   = (Double, Double, Double)
data Padding = Padding { _top    :: Double
                       , _right  :: Double
                       , _bottom :: Double
                       , _left   :: Double
                       } deriving (Show, Eq, Generic)

instance Default Padding where
    def = Padding 0.0 0.0 0.0 0.0


uniformPadding a = Padding a a a a
xyPadding x y    = Padding y x y x
