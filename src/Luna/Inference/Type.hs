{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Inference.Type where

import Flowbox.Prelude

import qualified Luna.Inference.Class    as Class
import           Luna.Inference.Class    (Class)
import           Data.Sequence           (Seq)
import qualified Data.Sequence           as Seq
--import           Luna.Inference.Instance
import qualified Data.Map                as Map
import           Data.Map                (Map)
import           Data.Repr
--import qualified Luna.Inference.RawData    as RawData
import           Luna.Inference.RawData

-- === Object ===


--type Object = Instance Type

-- utils


--object :: ToRawData a => a -> Object
--object a = Instance mempty (toRawData a)

--mkObject :: a -> Object
--mkObject a = Instance mempty (packRawData a)

-- instances

--instance Show Object where
--    showsPrec d obj = showParen (d > appPrec) $
--        showString "Object " . showsPrec (appPrec + 1) (obj ^. tp)
--        where appPrec = 10

--instance Repr Object where
--    repr obj = "Object"



data Lit = Int String
         -- |
         deriving (Show)


--class Instance a where
--    instantiate :: Lit -> a

--instance Instance Value where
--    instantiate = \case
--        Int i -> Value 0 (packRawData (read i :: Int)) -- FIXME: fix the 0 to real ID



class HasValue a where
    value :: Simple Lens a Value


type IsValue = Convertible Value

instantiate :: IsValue a => Lit -> a
instantiate = convert . \case
    Int i -> Value 0 (packRawData (read i :: Int)) -- FIXME: fix the 0 to real ID



data Value = Value ID RawData deriving (Show) -- its type id and computed data

data Object = Val Value
            | Cat Type
            deriving (Show)

instance Convertible Value Object where
    safeConvert = Right . Val

-- === Type ===

data Type = Class Class
          | Func  Function
          | Rec   Record
          deriving (Show)

type ID = Int

data TypeDesc = TypeDesc { _typeID :: ID
                         , _tp     :: Type
                         }

data Function = Function { _args :: Seq Arg } deriving (Show)

--data Value

data Arg = Arg { _name :: Maybe Text
               , _val  :: Maybe Object
               } deriving (Show)



-- record powinien miec id!
-- jak aplikujemy do funkcji cos typu Object i Object jest recordem to co ?
data Record = Record { _fields :: Map Text Field } deriving (Show)
data Field = Field Type deriving (Show)

makeLenses ''Function
makeLenses ''Record
makeLenses ''Arg



instance Monoid Record where
    mempty         = Record mempty
    o `mappend` o' = Record ( o ^. fields <> o' ^. fields )


-- Instances

instance Default Arg where
    def = Arg def def

