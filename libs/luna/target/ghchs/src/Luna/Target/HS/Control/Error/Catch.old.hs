---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Control.Error.Catch.old where

import Luna.Target.HS.Control.Error.Data

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

data ReRaise a = ReRaise deriving Show


------------------------------------------------------------------------
-- Catching
------------------------------------------------------------------------

class Catch value e h result | value e h -> result where
    catch :: (e -> h a) -> value a -> result a

-- === basic catching ===

instance Catch Safe a m Safe where catch _ = id

instance Catch (UnsafeBase base e) e Safe out <= (Monad out, Catch base e Safe out) where
    catch f a = case a of
        UnsafeValue a -> return a
        Error       e -> return . fromSafe $ f e
        UnsafeOther o -> catch f o

instance Catch (UnsafeBase base e1) e2 Safe (UnsafeBase dstBase e1) <= (Catch base e2 Safe dstBase) where
    catch f sa = case sa of
        UnsafeValue a -> UnsafeValue a
        Error       e -> Error e
        UnsafeOther o -> UnsafeOther $ catch f o


-- === re-raising ===

instance Catch (UnsafeBase base e1) e2 ReRaise (UnsafeBase base e1) where
    catch _ = id


-- === nested raising ===

instance Catch (UnsafeBase base e1) e2 (UnsafeBase base e1) (UnsafeBase dstBase e1) <= (Catch base e2 (UnsafeBase base e1) dstBase) where
    catch f sa = case sa of
        UnsafeValue a -> UnsafeValue a
        Error       e -> Error e
        UnsafeOther o -> UnsafeOther $ catch f o


instance Catch (UnsafeBase base e1) e1 (UnsafeBase base e2) (UnsafeBase base e2) where
    catch f sa = case sa of
        UnsafeValue a -> UnsafeValue a
        Error       e -> f e
        UnsafeOther o -> UnsafeOther o


instance Catch (UnsafeBase base e1) e1 (UnsafeBase base e1) (UnsafeBase base e1) where
    catch f sa = case sa of
        UnsafeValue a -> UnsafeValue a
        Error       e -> f e
        UnsafeOther o -> UnsafeOther o



instance Catch (UnsafeBase base e1) e2 (UnsafeBase base e3) (UnsafeBase dstBase e1) <= (Catch base e2 (UnsafeBase base e3) dstBase) where
    catch f sa = case sa of
        UnsafeValue a -> UnsafeValue a
        Error       e -> Error e
        UnsafeOther o -> UnsafeOther $ catch f o

