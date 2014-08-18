---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
!{-# LANGUAGE RightSideContexts #-}

module Data.TypeLevel.Set.Constrain where

import Data.TypeLevel.Set
import GHC.TypeLits
import Data.Typeable
import Data.TypeLevel.Bool
import Prelude hiding (Bool)

import Flowbox.Utils

--newtype Set a = Set a deriving (Show, Typeable)
--newtype ConstrainSet a = ConstrainSet a deriving (Show, Typeable)

newtype Include a = Include a deriving (Show, Typeable)
newtype Exclude a = Exclude a deriving (Show, Typeable)

--type C = (Include (Proxy 1), (Include (Proxy 2), ()))
--type S1 = Insert (Proxy 1) (Insert (Proxy 2) Empty)
--type S2 = Insert (Proxy 1) (Insert (Proxy 3) Empty)

type family Includes set where
    Includes () = ()
    Includes (Include x,xs) = (x,Includes xs)
    Includes (Exclude x,xs) = Includes xs

type family Excludes set where
    Excludes () = ()
    Excludes (Include x,xs) = Excludes xs
    Excludes (Exclude x,xs) = (x,Excludes xs)

type family MatchConstraint c set where
    MatchConstraint (Include x) ()     = False
    MatchConstraint (Include x) (x,xs) = True
    MatchConstraint (Include y) (x,xs) = MatchConstraint (Include y) xs

type family MatchConstraints cs set where
    MatchConstraints ()     set = True
    MatchConstraints (c,cs) set = IfThenElse (MatchConstraint c set) (MatchConstraints cs set) False


--type AssertTrue val = (val ~ True)
--type AssertFalse val = (val ~ False)

--foo :: AssertTrue a => a -> a
--foo = undefined

--main = do
--    printType $ foo (undefined :: False)
--    --printType (undefined :: AssertTrue (MatchConstraints (Include (Proxy 1), (Include (Proxy 2),())) S1 ))
--    print "end"