{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances,
             TemplateHaskell,
             UndecidableInstances #-}

module Workspace'.Vector.U'len where

import Common'.F_getx

len    v = (getx v) +1 
len''M v = return $ len v

