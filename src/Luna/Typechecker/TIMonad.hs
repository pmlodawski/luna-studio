module Luna.Typechecker.TIMonad (
    TI(..),
    TCLoggerT, TILogger,
    runTILogger, getNextID
  ) where


import Luna.Typechecker.Substitution (Subst)

import Logger (LoggerT(..),Log)

import Control.Applicative
import Control.Monad
import Data.Monoid



type TCLoggerT = LoggerT String
type TILogger = LoggerT String TI

newtype TI a = TI { runTI :: Subst -> Int -> (Subst, Int, a) }

runTILogger :: (Show e) => LoggerT e TI a -> (Subst, Int, (Either e a, [Log e]))
runTILogger x = runTI (runLoggerT x) mempty 0


getNextID :: TI Int
getNextID = TI $ \s i -> (s, i+1, i)


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