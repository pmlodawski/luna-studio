module Text.ScopeSearcher.Score where

import           Control.Lens
import           Data.Default


data Score = Score { _value   :: Double
                   , _penalty :: Double
                   } deriving (Show, Eq)

instance Ord Score where
    (Score a _) `compare` (Score b _) = a `compare` b

instance Default Score where
    def = Score def def

makeLenses ''Score

scale :: Double -> Score -> Score
scale factor score = score & value %~ (* factor)
