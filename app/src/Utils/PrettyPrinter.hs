module Utils.PrettyPrinter where

import Data.List   ( intercalate )
import Data.Maybe
import Data.Monoid ( (<>) )

class PrettyPrinter a where
    display :: a -> String

instance PrettyPrinter a => PrettyPrinter [a] where
    display xs = "[  " <> (intercalate " " $ fmap display xs) <> " ]"

instance PrettyPrinter a => PrettyPrinter (Maybe a) where
    display (Just v) = "J: " <> (display v)
    display Nothing  = "Nothing"

instance PrettyPrinter Int where
    display = show
