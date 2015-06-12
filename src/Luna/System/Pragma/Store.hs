{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Luna.System.Pragma.Store
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
----------------------------------------------------------------------------

module Luna.System.Pragma.Store where

import           Flowbox.Prelude     as P hiding (noneOf, lookup)

import qualified Luna.System.Pragma  as Pragma
import           Luna.System.Pragma  hiding (lookup, isEnabled)
import           Control.Monad.State (MonadState, StateT, runStateT, evalStateT)
import qualified Control.Monad.State as State
import           Text.Parser.Char        (string, noneOf, CharParsing)
import           Text.Parser.Token

----------------------------------------------------------------------
-- PragmaMap
----------------------------------------------------------------------

type    PragmaStore      = PragmaStoreT Identity
newtype PragmaStoreT m a = PragmaStoreT { unPragmaStoreT :: StateT PragmaMap m a }
                         deriving (Monad, MonadIO, MonadPlus, Applicative, Alternative, Functor)

class (Monad m, Applicative m) => MonadPragmaStore m where
    get :: m PragmaMap
    put :: PragmaMap -> m ()


-- == Instances ==

instance MonadState s m => MonadState s (PragmaStoreT m) where
    get = PragmaStoreT . lift $ State.get
    put = PragmaStoreT . lift . State.put

instance (Monad m, Functor m) => MonadPragmaStore (PragmaStoreT m) where
    get = PragmaStoreT $ State.get
    put = PragmaStoreT . State.put

instance (MonadTrans t, MonadPragmaStore m, Monad (t m), Applicative (t m)) => MonadPragmaStore (t m) where
    get = lift get
    put = lift . put
__overlapping__ = run get mempty

instance MonadPragmaStore m => MonadPragmaStore (StateT s m) where
    get = lift get
    put = lift . put


-- == State utils ==

runT :: PragmaStoreT m a -> PragmaMap -> m (a, PragmaMap)
runT = runStateT . unPragmaStoreT

run :: PragmaStore a -> PragmaMap -> (a,PragmaMap)
run = runIdentity .: runT

evalT :: Monad m => PragmaStoreT m a -> PragmaMap -> m a
evalT = evalStateT . unPragmaStoreT

eval :: PragmaStore a -> PragmaMap -> a
eval = runIdentity .: evalT

defrunT :: PragmaStoreT m a -> m (a, PragmaMap)
defrunT = flip runT def

defrun :: PragmaStore a -> (a, PragmaMap)
defrun = flip run def

-- == Pragma utils ==

type StoreCtx m = MonadPragmaStore m
type Ctx  t m a = (MonadPragmaStore m, IsPragma a, PragmaCons t)

parseByName :: (TokenParsing m, CharParsing m, MonadPragmaStore m) => Text -> m (Either AccessError ())
parseByName n = do
    res <- Pragma.parseByName n <$> get
    case res of
        Left  e -> return $ Left e
        Right p -> Right <$> (put =<< p) 

pragmaNames :: MonadPragmaStore m => m [Text]
pragmaNames = Pragma.pragmaNames <$> get

lookupByName :: StoreCtx m => Text -> m (Maybe HPragma)
lookupByName t = Pragma.lookupByName t <$> get

--

withStore :: (MonadPragmaStore m, Traversable t) => (PragmaMap -> t PragmaMap) -> m (t PragmaMap)
withStore f = do
    out <- f <$> get
    traverse put out
    return out

--

register :: Ctx t m a => PragmaDef t a -> m (Either RegisterError PragmaMap)
register = withStore . Pragma.register

push :: Ctx t m a => PragmaDef t a -> PragmaVal t a -> m (Either AccessError PragmaMap)
push = withStore .: Pragma.push

set :: Ctx t m a => PragmaDef t a -> PragmaVal t a -> m (Either AccessError PragmaMap)
set = withStore .: Pragma.set

lookup :: Ctx t m a => PragmaDef t a -> m (Either LookupError (PragmaInst t a))
lookup p = Pragma.lookup p <$> get

pop :: Ctx t m a => PragmaDef t a -> m (Either LookupError (PragmaInst t a))
pop p = do 
    lup <- Pragma.pop p <$> get
    traverse_ (put.snd) lup
    return $ fmap fst lup


----------------------------------------------------------------------
-- SwitchPragma
----------------------------------------------------------------------

enable :: (IsPragma a, MonadPragmaStore m) => SwitchPragma a -> m (Either AccessError PragmaMap)
enable = withStore . Pragma.enable

disable :: (IsPragma a, MonadPragmaStore m) => SwitchPragma a -> m (Either AccessError PragmaMap)
disable = withStore . Pragma.disable

isEnabled :: (Ctx t m a, PragmaVal t a ~ Switch) => PragmaDef t a -> m Bool
isEnabled p = do
    pDef <- lookup p
    return $ case fmap Pragma.isEnabled pDef of
        Right True -> True
        _          -> False

