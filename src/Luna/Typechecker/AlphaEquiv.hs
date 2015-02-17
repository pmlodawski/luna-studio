module Luna.Typechecker.AlphaEquiv (
    module Luna.Typechecker.AlphaEquiv.AlphaEquivMonad,
    AlphaEquiv(..), evalAlphaEquiv
  ) where


import            Flowbox.Prelude
import qualified  Data.Map.Strict                             as M
import            Data.Maybe
import Control.Monad (forM,zipWithM_)
import Data.List (permutations)

import            Luna.Typechecker.Data.TVar
import            Luna.Typechecker.AlphaEquiv.AlphaEquivMonad


class AlphaEquiv a where
    equiv         :: a -> a -> AlphaEquivMonad ()
    translateBtoA :: a -> AlphaEquivMonad a


instance AlphaEquiv TVar where
    equiv a b = ifM (ttEquiv a b)
                    (return ())
                    (ttInsert a b)
    translateBtoA b = ttBtoA b


evalAlphaEquiv :: AlphaEquiv a => a -> a -> Bool
evalAlphaEquiv a b = not.null $ runAlphaEq (equiv a b) M.empty M.empty
