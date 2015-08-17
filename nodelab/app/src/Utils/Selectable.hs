module Utils.Selectable where

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
