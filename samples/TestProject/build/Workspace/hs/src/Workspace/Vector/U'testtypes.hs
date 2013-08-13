-- This is Flowbox generated file.

module Workspace.Vector.U'testtypes where

-- imports
import           Flowbox.Luna.FClasses.U'print     
import           Flowbox.Luna.FClasses.U'select0   
import           Flowbox.Luna.Helpers.Core         

-- functions
testtypes' inputs'' = 
    let
        v'4 = "hello world!"
        v'2 = Console
        v'3 = v'2
        v'5 = (v'3, (v'4, ()))
        v'6 = print' (v'5)
        outputs'' = ()
        v'0 = inputs''
    in outputs''

testtypes'''M inputs'' = do
    let
        v'4 = "hello world!"
        v'2 = Console
        v'3 = v'2
        v'5 = (v'3, (v'4, ()))
    v'6 <- print'''M (v'5)
    let
        outputs'' = ()
        v'0 = inputs''
    return outputs''


