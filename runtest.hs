import Luna.Typechecker
import Luna.Typechecker.AST
import Luna.Typechecker.Substitution
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Type
import Luna.Typechecker.TypeEnv
import Luna.Typechecker.TypecheckClass
import Logger

import Control.Monad
import Data.Functor.Identity

runner :: (Inference a, Show a) => a -> IO ()
runner t = do let (subst, num, (res, stack)) = runTILogger (infer mkTypeEnv t)
              putStrLn   "######################################################################"
              putStrLn $ "typechecking: " ++ show t
              putStrLn $ show num ++ " type variables used"
              putStrLn   "-RES------------------------------------------------------------------"
              print    $ res
              putStrLn   "-SUBST----------------------------------------------------------------"
              print    $ subst
              putStrLn   "-STACK----------------------------------------------------------------"
              putStrLn $ formatStack True stack
              putStrLn   "######################################################################"
              putStrLn "\n\n\n"

main = do runner (ELit (LitDouble 10.5))
          runner (constBG ^. expr)