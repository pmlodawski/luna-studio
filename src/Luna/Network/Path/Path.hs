---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Path.Path(
    Path(..),
    empty,
    append,
    prepend,
    single,
    fromList,
    add,
    last,
    toString
) where
import Data.String.Utils (join)
import qualified Prelude
import Prelude hiding (last)

newtype Path = Path {segments :: [String]} deriving (Show, Ord, Eq)

empty :: Path
empty = Path []

single s = Path [s]


fromList :: [String] -> Path
fromList s = Path s


add :: Path -> Path -> Path
add (Path s1) (Path s2) = Path $ s1 ++ s2


append :: String -> Path -> Path
append segment path = Path $ (segments path) ++ [segment]


prepend :: String -> Path -> Path
prepend segment path = Path $ segment:(segments path)


last :: Path -> String
last path = Prelude.last $ segments path


toString :: Path -> String
toString path = join "." $ segments path


--instance Show Path where
--    show path = join "." $ segments path