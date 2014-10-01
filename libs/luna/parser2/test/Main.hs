import System.Environment (getArgs)
import Flowbox.Prelude
import qualified Luna.Parser.Parser as Parser
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty, (<>))
import Data.Default
import           System.IO                    (stdout)
import           Text.Show.Pretty
import qualified Luna.Parser.State as State


main = do
    args <- getArgs
    let path = args !! 0

    r <- Parser.parseFile path $ Parser.moduleParser ["x"] def
    case r of
        Left  e -> displayIO stdout $ Parser.renderErr e
        --Right a -> (putStrLn $ ppShow (fst a)) >> (putStrLn $ ppShow (snd a))
        Right a -> (putStrLn $ ppShow (fst a)) >> (putStrLn $ ppShow (view State.namespace $ snd a))