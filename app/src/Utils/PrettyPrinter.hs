module Utils.PrettyPrinter where

import Data.List   ( intercalate )
import Data.Maybe
import Data.Dynamic
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

instance PrettyPrinter Integer where
    display = show

instance PrettyPrinter Double where
    display = show

instance PrettyPrinter Char where
    display = show

instance PrettyPrinter String where
    display = show

instance PrettyPrinter Dynamic where
    display _ = "Dynamic"

instance (PrettyPrinter a, PrettyPrinter b, PrettyPrinter c, PrettyPrinter d) => PrettyPrinter (a, b, c, d) where
    display (a, b, c, d) = "(" <> display a <> "," <> display b <> "," <> display c <> "," <> display d <> ")"
