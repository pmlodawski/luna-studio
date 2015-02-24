module Luna.Typechecker.AlphaEquiv (
    module Luna.Typechecker.AlphaEquiv.AlphaEquivMonad,
    AlphaEquiv(..), evalAlphaEquiv,
    nonDeterministicEquiv
  ) where


import            Flowbox.Prelude
import qualified  Data.Map.Strict                             as M
import            Data.Maybe
import Control.Monad (forM,zipWithM_)
import Data.List (permutations)

import Data.Map.IntConvertibleSet (IntConvertibleSet)
import qualified Data.Map.IntConvertibleSet as S

import            Luna.Typechecker.Data.TVar
import            Luna.Typechecker.AlphaEquiv.AlphaEquivMonad


class AlphaEquiv a where
    equiv         :: a -> a -> AlphaEquivMonad ()
    translateBtoA :: a -> AlphaEquivMonad a
    freevars      :: a -> IntConvertibleSet TVar

instance (AlphaEquiv a) => AlphaEquiv [a] where
    equiv a b = zipWithM_ equiv a b
    translateBtoA = mapM translateBtoA
    freevars = mconcat . fmap freevars

instance AlphaEquiv TVar where
    equiv a b       = do  c <- ttEquiv a b
                          unless c $ ttInsert a b
    translateBtoA b = ttBtoA b
    freevars        = S.singleton


evalAlphaEquiv :: AlphaEquiv a => a -> a -> Bool
evalAlphaEquiv a b = not.null $ runAlphaEq (equiv a b) M.empty M.empty

nonDeterministicEquiv :: (AlphaEquiv a, Show a) => [a] -> [a] -> AlphaEquivMonad ()
nonDeterministicEquiv x y
  | length x <= length y = fork [ equiv x' y  | x' <- permutations x]
  | otherwise            = fork [ equiv x  y' | y' <- permutations y]
