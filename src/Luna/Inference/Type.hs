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
import           Data.Convert
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




--class Instance a where
--    instantiate :: Lit -> a

--instance Instance Value where
--    instantiate = \case
--        Int i -> Value 0 (packRawData (read i :: Int)) -- FIXME: fix the 0 to real ID



    --class HasValue a where
    --    value :: Simple Lens a Value


    --type IsValue = Convertible Value

--instantiate :: IsValue a => Lit -> a
--instantiate = convert . \case
--    Int i -> Value 0 (packRawData (read i :: Int)) -- FIXME: fix the 0 to real ID




    --data Value = Value ID RawData
    --           | Value Type
    --           deriving (Show) -- its type id and computed data

    --data ObjectType = Val Value
    --                | Var Type
    --                deriving (Show)

--data Object = Object Type ObjectType



    --instance Convertible Value Object where
    --    convert = Val


    --data Literal = Int Int
    --             | Float Float
    --             deriving (Show)

-- === Type ===

--a = Int -- Class (Record "Int")
--b = 2   -- Value (Lit 2) (Record "Int") - NIE, powinno byc

--tak: (dopisane by zauwazyc, zobacz notatki):
--b = 2   -- Value (Lit 2) (Var ..) - bo nie zrobilismy defaultingu! Tzn na pewno jest to Int, chyba ze ktos uzyl funkcji wymuszajacej inny typ i ten typ jest jednoznaczny (nie ma 2 funkcji wymuszajacych rozne typy)


--a = 2   -- Lit 2   -   default category = Int
--b = Int -- Rec "Int"

---- <name> === qualified name
---- {x}    === name pointer, e.g. de brougle

--c = Foo 1 2 -- App (Cons <Foo>) [Lit 1, Lit 2]

--c = Foo 1 bar -- App (Cons <Foo>) [Lit 1, Var {bar}]
--              -- {bar} wskazuje na kopie grafu powiazan funkcji bar. Jezeli tu podstawimy dane, jak np. Int, tworzymy bardziej wyspecjalizowany graf, ktory cachujemy.


--              -- {bar} moze byc pointerem na typ funkcji ktory mogl jeszcze nie byc najdokladniej przeliczony
--              -- po najdokladniejszym przeliczeniu mozemy podstawiac zmienne. Po ich podstawieniu liczymy cache!
--              -- tak wiec typy sa zapamietywane z roznymi rozwiazaniami - np "bar" z przekazanym Intem


data Object = Value Type
            | Class Type
            deriving (Show)

data Type = Type deriving (Show)
    --data Value = Class Class
    --           | Func  Function
    --           | Rec   Record
    --           | Lit   Literal
    --           deriving (Show)


    --data Value = Value Data Type

    --data Data = Lit Literal
    --          | Func

    --type ID = Int

    --data TypeDesc = TypeDesc { _typeID :: ID
    --                         , _tp     :: Type
    --                         }

data Function = Function { _args :: Seq Arg2 } deriving (Show)

    ----data Value

data Arg2 = Arg2 { _name :: Maybe Text
               , _val  :: Maybe Object
               } deriving (Show)

--makeLenses ''Arg2


-- record powinien miec id!
-- jak aplikujemy do funkcji cos typu Object i Object jest recordem to co ?
    --data Record = Record { _fields :: Map Text Field } deriving (Show)
    --data Field = Field Type deriving (Show)

    --makeLenses ''Function
    --makeLenses ''Record
    --makeLenses ''Arg



    --instance Monoid Record where
    --    mempty         = Record mempty
    --    o `mappend` o' = Record ( o ^. fields <> o' ^. fields )


    ---- Instances

instance Default Arg2 where
    def = Arg2 def def



