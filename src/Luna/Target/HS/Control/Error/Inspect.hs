---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Control.Error.Inspect where

import Luna.Target.HS.Control.Error.Data


------------------------------------------------------------------------
-- DataTypes
------------------------------------------------------------------------

data ErrorCheck a = CheckOK a
                  | forall e. Show e => CheckFail e


------------------------------------------------------------------------
-- Type classes
------------------------------------------------------------------------

class HandleError a val | a -> val where
    handleError :: a -> ErrorCheck val


------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------

printChecked check = case check of
    CheckOK _   -> return ()
    CheckFail e -> print $ "*** Exception: Unhandled error " ++ show e


printCheck = printChecked . handleError


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance HandleError (Safe a) a where
    handleError (Safe a) = CheckOK a


instance HandleError (UnsafeBase base err val) val <= (HandleError (base val) val, Show err) where
    handleError a = case a of
        UnsafeValue val -> CheckOK val
        Error e         -> CheckFail e
        UnsafeOther o   -> handleError o

