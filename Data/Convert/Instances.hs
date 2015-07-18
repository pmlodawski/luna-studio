{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

{- |
   Module     : Data.Convertible.Instances
   Copyright  : Copyright (C) 2009-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Collection of ready-made 'Data.Convertible.Convertible' instances.  See
each individual module for more docs:

"Data.Convertible.Instances.C"

"Data.Convertible.Instances.Map"

"Data.Convertible.Instances.Num"

"Data.Convertible.Instances.Time"

You can find a list of these instances at 'Data.Convertible.Base.Convertible'.
-}

module Data.Convert.Instances () where

import Data.Convert.Instances.Default ()

import Data.Convert.Instances.C       ()
import Data.Convert.Instances.Map     ()
import Data.Convert.Instances.Num     ()
import Data.Convert.Instances.Text    ()
import Data.Convert.Instances.Time    ()
import Data.Convert.Instances.Time    ()
