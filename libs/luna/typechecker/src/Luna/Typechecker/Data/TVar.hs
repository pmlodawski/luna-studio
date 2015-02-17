module Luna.Typechecker.Data.TVar where


import Flowbox.Prelude

import Data.Packable    (Pack(pack), Unpack(unpack), Packable)


newtype TVar  = TVar { fromTVar :: Int } deriving (Eq, Ord)

makePrisms ''TVar

instance Show TVar          where show = show . fromTVar
instance Unpack TVar Int    where unpack = fromTVar
instance Pack   Int TVar    where pack   = TVar
instance Packable TVar Int

