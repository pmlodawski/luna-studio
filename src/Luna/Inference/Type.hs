{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Inference.Type where

import Flowbox.Prelude

import qualified Luna.Inference.Class    as Class
import           Luna.Inference.Class    (Class)
import           Data.Sequence           (Seq)
import qualified Data.Sequence           as Seq
import           Luna.Inference.Instance
import qualified Data.Map                as Map
import           Data.Map                (Map)
import           Data.Repr
import qualified Luna.Inference.Data     as Data
import           Luna.Inference.Data

-- === Object ===

type Object = Instance Record


-- utils

object t a = Instance t (fromValue a)
unpack = Data.unpack . view val

mkObject :: a -> Object
mkObject = object mempty

-- instances

instance Show Object where
    showsPrec d obj = showParen (d > appPrec) $
        showString "Object " . showsPrec (appPrec + 1) (obj ^. tp)
        where appPrec = 10

instance Repr Object where
    repr obj = "Object"






-- === Type ===

data Type = Class Class
          | Func  Function


data Function = Function { _args :: Seq Arg }

data Arg = Arg { _name :: Maybe Text
               , _val  :: Maybe Object
               }

arg :: Text -> Arg
arg = flip Arg def . Just

argOf :: Text -> Object -> Arg
argOf n v = Arg (Just n) (Just v)


data Record = Record { _fields :: Map Text Field }
data Field = Field Type

makeLenses ''Function
makeLenses ''Record

arity :: Function -> Int
arity = Seq.length . view args

instance Monoid Record where
    mempty         = Record mempty
    o `mappend` o' = Record ( o ^. fields <> o' ^. fields )


-- Instances

instance Default Arg where
    def = Arg def def

-- The show instances are awkward because the Show instance
-- for Object is defined in the Object.hs file and
-- it is not imported here in order to prevent from circular dependencies
deriving instance Show Object => Show Function
deriving instance Show Object => Show Arg
deriving instance Show Object => Show Type
deriving instance Show Object => Show Record
deriving instance Show Object => Show Field