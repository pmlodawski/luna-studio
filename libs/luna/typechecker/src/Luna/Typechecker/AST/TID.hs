module Luna.Typechecker.AST.TID (
    enumTID, TID(..)
  ) where


import Luna.Typechecker.AST.Internal.TID


enumTID :: Int -> TID
enumTID n = TID $ "v" ++ show n
