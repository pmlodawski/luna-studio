---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Transform.AST.TxtParser.Token where

import Flowbox.Prelude
import Flowbox.Luna.Data.AST.SourcePos (SourceRange(SourceRange), SourcePos)
import Flowbox.Luna.Data.AST.Utils     (ID)

data Token a = Token { id    :: Maybe ID
                     , value :: a
                     , range :: SourceRange
                     }
             deriving (Show)


mk :: a -> SourcePos -> SourcePos -> Token a
mk val begin end = Token Nothing val $ SourceRange (Just begin) (Just end)


flatten :: [Token a] -> Token [a]
flatten a = foldr append mempty a

append :: Token a -> Token [a] -> Token [a]
append t1 t2 = Token Nothing (value t1 : value t2) (mappend (range t1) (range t2))

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid a => Monoid (Token a) where
	mempty        = Token Nothing mempty mempty
	mappend t1 t2 = Token Nothing (mappend (value t1) (value t2)) (mappend (range t1) (range t2))


instance Functor Token where
	fmap f tok = tok { value = f $ value tok }
