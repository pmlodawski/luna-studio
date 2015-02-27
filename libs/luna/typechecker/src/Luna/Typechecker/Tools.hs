module Luna.Typechecker.Tools (
    without
  ) where


import Data.List
import Flowbox.Prelude hiding (without)


without :: (Eq a) => [a] -> [a] -> [a]
without = (\\)
