module Luna.Interpreter.Session where

import qualified Language.Haskell.Session as HS
import Unsafe.Coerce
import qualified Language.Haskell.Session.Hint.Eval as HEval
import Prologue
import GHC.Prim (Any)



type Name = String
type Type = String



findSymbol :: Name -> Type -> HS.Session Any
findSymbol n t =  unsafeCoerce <$> HEval.interpretTyped n t
-- applyStringArg :: String -> Any -> Any
-- appArg 2 f
appArg :: Any -> Any -> Any
appArg = unsafeCoerce

unsafeCast :: Any -> a
unsafeCast = unsafeCoerce




test :: IO ()
test = HS.run $ do
    HS.setImports ["Prelude"]
    length <- findSymbol "length" "String -> Int"
    let r = appArg length (unsafeCoerce "bla")
    print (unsafeCast r :: Int)
    return ()
