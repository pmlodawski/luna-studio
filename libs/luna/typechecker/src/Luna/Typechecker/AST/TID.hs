module Luna.Typechecker.AST.TID (
    enumTID, TID
  ) where

type TID = String


enumTID :: Int -> TID
enumTID n = "v" ++ show n
