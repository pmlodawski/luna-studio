-- This is Flowbox generated file.

module Workspace.Vector.U'init where

-- imports
import Flowbox.Luna.FClasses.U'select0
import Flowbox.Luna.FClasses.U'select1
import Flowbox.Luna.FClasses.U'select2
import Flowbox.Luna.FClasses.U'select3
import Flowbox.Luna.FClasses.U'x'setter
import Flowbox.Luna.FClasses.U'y'setter
import Flowbox.Luna.FClasses.U'z'setter
import Flowbox.Luna.Helpers.Core

-- functions
init' inputs'' = 
    let
        v'0 = inputs''
        v'4 = select3' (v'0)
        v'3 = select2' (v'0)
        v'2 = select1' (v'0)
        v'1 = select0' (v'0)
        v'5 = (v'1, (v'2, ()))
        v'6 = x'setter' (v'5)
        v'7 = select0' (v'6)
        v'8 = (v'7, (v'3, ()))
        v'9 = y'setter' (v'8)
        v'10 = select0' (v'9)
        v'11 = (v'10, (v'4, ()))
        v'12 = z'setter' (v'11)
        outputs'' = v'12
    in outputs''

init'''M inputs'' = return $ init' inputs''

