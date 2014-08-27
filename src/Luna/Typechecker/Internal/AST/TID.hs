module Luna.Typechecker.Internal.AST.TID (enumTID, TID) where




-- | Type identifier.
-- Example values: "Int", "(,)", "(,)", "type123456" (automatically generated).
type TID = String


enumTID :: Int -> TID
enumTID n = "tid_" ++ show n
