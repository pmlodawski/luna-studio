module Utils.PrettyPrinter where

import           Prelude
import           Data.List      ( intercalate )
import           Data.Maybe
import           Data.Dynamic
import           Data.Monoid    ( (<>) )
import           Text.Printf

import           Data.Text.Lazy ( Text )

class PrettyPrinter a where
    display :: a -> String

instance PrettyPrinter a => PrettyPrinter [a] where
    display xs = "[" <> (intercalate " " $ fmap display xs) <> "]"

instance PrettyPrinter a => PrettyPrinter (Maybe a) where
    display (Just v) = "J " <> (display v)
    display Nothing  = "Nothing"

instance (PrettyPrinter a, PrettyPrinter b) => PrettyPrinter (a, b) where
    display (x, y) = "(" <> display x <> "," <> display y <> ")"

instance (PrettyPrinter a, PrettyPrinter b, PrettyPrinter c) => PrettyPrinter (a, b, c) where
    display (x, y, z) = "(" <> display x <> "," <> display y <> "," <> display z <> ")"

instance PrettyPrinter Int where
    display = show

instance PrettyPrinter Integer where
    display = show

instance PrettyPrinter Double where
    display v = show $ (fromIntegral (round $ v * 1000.0)) / 1000.0

instance PrettyPrinter Char where
    display = show

instance PrettyPrinter String where
    display = show

instance PrettyPrinter Text where
    display = show

instance PrettyPrinter Bool where
    display True  = "T"
    display False = "F"

instance PrettyPrinter Dynamic where
    display _ = "Dynamic"

instance (PrettyPrinter a, PrettyPrinter b, PrettyPrinter c, PrettyPrinter d) => PrettyPrinter (a, b, c, d) where
    display (a, b, c, d) = "(" <> display a <> "," <> display b <> "," <> display c <> "," <> display d <> ")"
