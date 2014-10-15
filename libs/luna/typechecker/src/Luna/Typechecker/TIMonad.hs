module Luna.Typechecker.TIMonad (
    TI(..),
    TCLoggerT,
    module Logger
  ) where


import Luna.Typechecker.Substitution
import Logger

import Control.Applicative (Applicative(..))
import Control.Monad (ap)


type TCLoggerT = LoggerT String

newtype TI a = TI { runTI :: Subst -> Int -> (Subst, Int, a) }


instance Functor TI where
  fmap f (TI sia) = TI $ \s i -> let (s', i', a') = sia s i in (s', i', f a')

instance Applicative TI where
  pure = return
  (<*>) = ap

instance Monad TI where
  return x   = TI (\s n -> (s,n,x))
  TI f >>= g = TI (\s n -> case f s n of
                             (s',n',x) -> let TI gx = g x
                                           in gx s' n')