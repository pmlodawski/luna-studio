module Luna.Typechecker.Internal.Assumptions (Assump(..), find) where

import qualified Luna.Typechecker.Internal.AST.Scheme       as Sch

import qualified Luna.Typechecker.Internal.Substitutions    as Sub

import           Luna.Typechecker.Internal.AST.TID          (TID)



data Assump = TID :>: Sch.Scheme
            deriving (Show)

instance Sub.Types Assump where
  apply s (i :>: sc) = i :>: (Sub.apply s sc)
  tv (_ :>: sc) = Sub.tv sc

-- TODO [kgdk] 19 sie 2014: wybrać lepsze
--find i lst = case Prelude.find (\(i' :>: sc) -> i' == i) lst of
--               Just (i' :>: sc) -> return sc
--               Nothing          -> fail ("unbound identifier: " ++ i)
find :: Monad m => TID -> [Assump] -> m Sch.Scheme
find i [] = fail ("unbound identifier: " ++ i)
find i ((i' :>: sc) : as) = if i==i'
                              then return sc
                              else find i as
