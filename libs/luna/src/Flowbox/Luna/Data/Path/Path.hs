---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Path.Path (
    Path(..),
    empty,
    append,
    prepend,
    single,
    fromList,
    toList,
    add,
    head,
    tail,
    init,
    last,
    --toString,
) where

import           Flowbox.Prelude hiding (empty, head, init, last, tail)
import qualified Flowbox.Prelude as Prelude

newtype Path = Path {segments :: [String]} deriving (Show, Ord, Eq)

empty :: Path
empty = Path []


single :: String -> Path
single s = Path [s]


fromList :: [String] -> Path
fromList s = Path s

toList :: Path -> [String]
toList = segments

add :: Path -> Path -> Path
add (Path s1) (Path s2) = Path $ s1 ++ s2


append :: String -> Path -> Path
append segment path = Path $ (segments path) ++ [segment]


prepend :: String -> Path -> Path
prepend segment path = Path $ segment:(segments path)


head :: Path -> String
head path = Prelude.head $ segments path


tail :: Path -> Path
tail path = Path $ Prelude.tail $ segments path


init :: Path -> Path
init path = Path $ Prelude.init $ segments path


last :: Path -> String
last path = Prelude.last $ segments path


--toString :: Path -> String
--toString path = join "." $ segments path






--instance Show Path where
--    show path = join "." $ segments path
