---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Path.Path(
    Path(..),
    empty,
    append,
    prepend,
    single,
    fromList,
    toList,
    add,
    last,
    --toString,
    init,
) where

import qualified Prelude   
import qualified Prelude   
import           Prelude hiding (last, init)

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


last :: Path -> String
last path = Prelude.last $ segments path


--toString :: Path -> String
--toString path = join "." $ segments path


init :: Path -> Path
init path = Path $ Prelude.init (segments path)




--instance Show Path where
--    show path = join "." $ segments path