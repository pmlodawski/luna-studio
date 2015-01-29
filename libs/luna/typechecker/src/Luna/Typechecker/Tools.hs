module Luna.Typechecker.Tools (
    without
  ) where


import Data.List


without :: (Eq a) => [a] -> [a] -> [a]
without = (\\)
