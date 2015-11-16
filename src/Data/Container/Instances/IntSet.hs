module Data.Container.Instances.IntSet where


import Prologue

import           Data.Container.Class
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

type instance Item IntSet = Int

instance ToList IntSet where toList = IntSet.toList