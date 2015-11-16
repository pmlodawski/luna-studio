module Luna.Syntax.AST.Lit where

import           Flowbox.Prelude hiding (Repr,repr)
import qualified Data.Text.AutoBuilder as Text
import Data.Reprx


data Lit = Int    Int
         | String Text.AutoBuilder
         deriving (Show)

instance IsString Lit where
    fromString = String . fromString

instance Repr s Lit where
    repr = \case
        Int    i -> "Int"    <+> repr i
        String s -> "String" <+> fromString (show s)