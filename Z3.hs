
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

import Z3.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.Traversable
import Prelude hiding (sequence)

main = do
    out <- evalZ3 $ do
        is  <- mkIntSort
        _3  <- mkInteger 3
        _5  <- mkInteger 5
        _10 <- mkInteger 10
        x   <- mkFreshIntVar "x"
        y   <- mkFreshIntVar "y"
        y2  <- mkBvmul y y

        assert =<< mkAnd =<< sequence
            [ mkLe x _5
            , mkGe x _3
            , mkLe y _10
            , mkEq x y2
            ]
        --solverCheck
        return ()
        (res, Just m) <- getModel
        (res,) <$> modelToString m

        withModel $ \m -> eval m x

    --putStrLn out
    print out
    print "end"