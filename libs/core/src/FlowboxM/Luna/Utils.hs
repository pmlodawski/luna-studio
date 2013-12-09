module FlowboxM.Luna.Utils where

import FlowboxM.Luna.Data

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
-- f .: g = \x y->f (g x y)
-- f .: g = (f .) . g
-- (.:) f = ((f .) .)
-- (.:) = (.) (.) (.)
(.:) = (.) . (.)

liftFPure1  f (Pure a) = Pure $ f a
liftFPure2  f (Pure a) = liftFPure1 (f a)
liftFPure3  f (Pure a) = liftFPure2 (f a)
liftFPure4  f (Pure a) = liftFPure3 (f a)
liftFPure5  f (Pure a) = liftFPure4 (f a)
liftFPure6  f (Pure a) = liftFPure5 (f a)
liftFPure7  f (Pure a) = liftFPure6 (f a)
liftFPure8  f (Pure a) = liftFPure7 (f a)
liftFPure9  f (Pure a) = liftFPure8 (f a)
liftFPure10 f (Pure a) = liftFPure9 (f a)


liftFIO1  f (Pure a) = f a
liftFIO2  f (Pure a) = liftFIO1 (f a)
liftFIO3  f (Pure a) = liftFIO2 (f a)
liftFIO4  f (Pure a) = liftFIO3 (f a)
liftFIO5  f (Pure a) = liftFIO4 (f a)
liftFIO6  f (Pure a) = liftFIO5 (f a)
liftFIO7  f (Pure a) = liftFIO6 (f a)
liftFIO8  f (Pure a) = liftFIO7 (f a)
liftFIO9  f (Pure a) = liftFIO8 (f a)
liftFIO10 f (Pure a) = liftFIO9 (f a)


mkPure0  a = Pure $ a
mkPure1  a v1 = Pure $ a v1
mkPure2  a v1 v2 = Pure $ a v1 v2
mkPure3  a v1 v2 v3 = Pure $ a v1 v2 v3
mkPure4  a v1 v2 v3 v4 = Pure $ a v1 v2 v3 v4
mkPure5  a v1 v2 v3 v4 v5 = Pure $ a v1 v2 v3 v4 v5
mkPure6  a v1 v2 v3 v4 v5 v6 = Pure $ a v1 v2 v3 v4 v5 v6
mkPure7  a v1 v2 v3 v4 v5 v6 v7 = Pure $ a v1 v2 v3 v4 v5 v6 v7
mkPure8  a v1 v2 v3 v4 v5 v6 v7 v8 = Pure $ a v1 v2 v3 v4 v5 v6 v7 v8
mkPure9  a v1 v2 v3 v4 v5 v6 v7 v8 v9 = Pure $ a v1 v2 v3 v4 v5 v6 v7 v8 v9
mkPure10 a v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = Pure $ a v1 v2 v3 v4 v5 v6 v7 v8 v9 v10


defFunction0 f = f

defFunction1 f v1 = do
    v1' <- getIO v1
    f (Pure v1')

defFunction2 f v1 v2 = do
    v1' <- getIO v1
    v2' <- getIO v2
    f (Pure v1') (Pure v2')

defFunction3 f v1 v2 v3 = do
    v1' <- getIO v1
    v2' <- getIO v2
    v3' <- getIO v3
    f (Pure v1') (Pure v2') (Pure v3')

defFunction4 f v1 v2 v3 v4 = do
    v1' <- getIO v1
    v2' <- getIO v2
    v3' <- getIO v3
    v4' <- getIO v4
    f (Pure v1') (Pure v2') (Pure v3') (Pure v4')

defFunction5 f v1 v2 v3 v4 v5 = do
    v1' <- getIO v1
    v2' <- getIO v2
    v3' <- getIO v3
    v4' <- getIO v4
    v5' <- getIO v5
    f (Pure v1') (Pure v2') (Pure v3') (Pure v4') (Pure v5')
