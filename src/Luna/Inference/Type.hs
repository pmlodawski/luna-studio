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
--import qualified Luna.Inference.Value    as Value
import           Luna.Inference.Value

-- === Object ===

type Object = Instance Record


-- utils


object :: ToValue a => a -> Object
object a = Instance mempty (toValue a)

mkObject :: a -> Object
mkObject a = Instance mempty (packRawData a)

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
          | Rec   Record
          deriving (Show)


data Function = Function { _args :: Seq Arg } deriving (Show)

data Arg = Arg { _name :: Maybe Text
               , _val  :: Maybe Object
               } deriving (Show)

arg :: Text -> Arg
arg = flip Arg def . Just

argOf :: Text -> Object -> Arg
argOf n v = Arg (Just n) (Just v)


data Record = Record { _fields :: Map Text Field } deriving (Show)
data Field = Field Type deriving (Show)

makeLenses ''Function
makeLenses ''Record



instance Monoid Record where
    mempty         = Record mempty
    o `mappend` o' = Record ( o ^. fields <> o' ^. fields )


-- Instances

instance Default Arg where
    def = Arg def def

