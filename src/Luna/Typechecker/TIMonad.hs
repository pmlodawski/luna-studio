module Luna.Typechecker.TIMonad (
    TI(..),
    TCLoggerT, TILogger,
    module Logger,
    runTILogger
  ) where


import Luna.Typechecker.Substitution
import Logger

import Control.Applicative (Applicative(..))
import Control.Monad (ap)
import Data.Monoid


type TCLoggerT = LoggerT String
type TILogger = LoggerT String TI

newtype TI a = TI { runTI :: Subst -> Int -> (Subst, Int, a) }


runTILogger x = runTI (runLoggerT x) mempty 0


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