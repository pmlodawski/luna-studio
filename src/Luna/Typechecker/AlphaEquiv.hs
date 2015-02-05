module Luna.Typechecker.AlphaEquiv (
    module Luna.Typechecker.AlphaEquiv.AlphaEquivMonad,
    AlphaEquiv(..), evalAlphaEquiv
  ) where


import            Flowbox.Prelude
import qualified  Data.Map.Strict                             as M
import            Data.Maybe

import            Luna.Typechecker.AlphaEquiv.AlphaEquivMonad


class AlphaEquiv a where
    equiv :: a -> a -> AlphaEquivMonad ()


evalAlphaEquiv :: AlphaEquiv a => a -> a -> Bool
evalAlphaEquiv a b = isJust $ extract $ runAlphaEq (equiv a b) M.empty M.empty M.empty M.empty
  where extract (x,fmab,fmba,ttab,ttba) = x