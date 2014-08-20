module Flowbox.Luna.Typechecker.Internal.AST.TID (TID(..)) where

-- | Type identifier.
-- Example values: "Int", "(,)", "(,)", "type123456" (automatically generated).
type TID = String

-- TODO [kgdk] 14 sie 2014: implement types generator
-- TODO [kgdk] 14 sie 2014: change implementation to use something different than String so that
-- comparisons would be faster. Proposal: use Int with additional `:: Map Int String` in TI monad.



--enumTID :: Int -> TID
--enumTID n = "tid" ++ show n