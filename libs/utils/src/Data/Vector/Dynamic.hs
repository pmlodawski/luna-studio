module Data.Vector.Dynamic where

import Prelude

import qualified Data.Vector.Mutable as Vector
import           Data.Vector.Mutable (MVector)

import Control.Monad.ST

tst = do
    let elems' = runST $ do
        mv' <- Vector.new 100 :: ST s (MVector s Int)
        --mapM (\i -> MV.unsafeWrite mv' i i) [0..j]
        --elems <- mapM (MV.read mv') [0..i]
        return ()

    print "hello"