module Object.Object where

import Data.Dynamic
import Data.Typeable
import Data.Monoid
import Data.Default
import Control.Applicative
import Control.Lens

import Utils.Wrapper
import Utils.PrettyPrinter

newtype Object a = Object { fromObject :: a } deriving (Eq, Show)

makeLenses ''Object

instance Unwrap Object where unwrap = fromObject

instance PrettyPrinter a => PrettyPrinter (Object a) where
    display (Object o) = "o( " <> display o <> " )"

type ID = Int


class Selectable a where
    setSelected       :: a -> Bool -> a
    isSelected        :: a -> Bool
    select            :: a -> a
    unselect          :: a -> a
    toggleSelection   :: a -> a
    select o          = setSelected o True
    unselect o        = setSelected o False
    toggleSelection o = setSelected o $ not . isSelected $ o

instance Selectable a => Selectable (Object a) where
    setSelected (Object o) = Object . setSelected o
    isSelected  (Object o) = isSelected o
