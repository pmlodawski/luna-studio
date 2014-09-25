module Luna.Typechecker.Assumptions (
    Assump(..), find
  ) where


import Luna.Typechecker.Substitutions (Types(..))

import Luna.Typechecker.AST.Scheme    (Scheme(..))
import Luna.Typechecker.AST.TID       (TID)

import Luna.Typechecker.Internal.Logger

import Control.DeepSeq

import Data.List                      (intercalate)


data Assump = TID :>: Scheme
            deriving (Eq)


instance NFData Assump where
  rnf (tid :>: sch) = rnf tid `seq` rnf sch

instance Show Assump where
  show (t :>: sch) = show t ++ " :: " ++ show sch
  showList as s = s ++ intercalate "\n" (map show as)

instance Types Assump where
  apply s (i :>: sc) = i :>: apply s sc
  tv (_ :>: sc) = tv sc


find :: (Monad m) => TID -> [Assump] -> TCLoggerT m Scheme
find i [] = throwError $ "unbound identifier: " ++ i
find i ((i' :>: sc) : as) = if i==i'
                              then return sc
                              else find i as
