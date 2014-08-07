---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module InterpreterSpec where

import Test.Hspec

import Flowbox.Prelude


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "interpreter" $ do
        it "works"$
            pending
