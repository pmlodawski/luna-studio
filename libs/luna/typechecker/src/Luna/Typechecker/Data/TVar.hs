{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Typechecker.Data.TVar where


import Control.Lens

import Data.Wrapper         (Pack(..), Unpack(..))


newtype TVar  = TVar { fromTVar :: Int } deriving (Eq, Ord)

makePrisms ''TVar


instance Show TVar where show = show . fromTVar

instance Unpack TVar Int  where unpack = fromTVar
instance Pack   Int TVar  where pack   = TVar
