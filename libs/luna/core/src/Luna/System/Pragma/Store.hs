{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

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

import qualified Luna.System.Pragma           as Pragma
import           Luna.System.Pragma           hiding (lookup, isEnabled)
import qualified Control.Monad.State          as State
import           Text.Parser.Char             (string, noneOf, CharParsing)
import           Text.Parser.Token
import           Control.Monad.State.Generate (newState)

----------------------------------------------------------------------
-- PragmaMap
----------------------------------------------------------------------

$(newState "PragmaStore" ''PragmaMap)

--defrunT :: PragmaStoreT m a -> m (a, PragmaMap)
--defrunT = flip runT def

--defrun :: PragmaStore a -> (a, PragmaMap)
--defrun = flip run def

-- == Pragma utils ==

--instance MonadTrans PragmaStoreT

deriving instance MonadTrans PragmaStoreT

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

