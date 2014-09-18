module Luna.Typechecker.AST.TID (
    enumTID, TID
  ) where


-- | Type identifier.
-- Example values: "Int", "(,)", "(,)", "type123456" (automatically generated).
type TID = String


enumTID :: Int -> TID
enumTID n = "v" ++ show n
