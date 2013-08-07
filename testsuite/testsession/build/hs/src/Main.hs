-- imports
import Common'.C''print
import Flowbox'.Core
import Flowbox'.System.Console (Console(..))
import qualified Flowbox'.System.Console as Console
import Prelude(return)

-- datatypes


-- functions
mymain inputs' = 
    let
        v'4 = "hello world!"
        v'2 = Console
        v'3 = v'2
        v'5 = (v'3, (v'4, ()))
        v'6 = print v'5
        outputs' = ()
        v'0 = inputs'
    in outputs'

mymain''M inputs' = do
    let
        v'4 = "hello world!"
        v'2 = Console
        v'3 = v'2
        v'5 = (v'3, (v'4, ()))
    v'6 <- print''M v'5
    let
        outputs' = ()
        v'0 = inputs'
    return outputs'


main = mymain''M ()