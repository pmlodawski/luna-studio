module Luna.Syntax.Symbol.QualPath where

import qualified Data.String.Utils as String
import           FastString
import qualified FastString
import           Prologue


type QualPath = [FastString] --TODO[PM] Use FastStrings


mk :: ToString s => s -> QualPath
mk = map FastString.mkFastString . String.split "." . toString


instance IsString QualPath where
    fromString = mk
