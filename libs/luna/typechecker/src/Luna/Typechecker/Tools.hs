module Luna.Typechecker.Tools (
    without
  ) where


without :: (Eq a) => [a] -> [a] -> [a]
without [] a    = []
without (x:a) b = if x `elem` b then without a b
                  else x : without a b