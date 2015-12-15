---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}

module Flowbox.Prelude(
    module Flowbox.Prelude,
    module Prelude,
    module X
) where

import Prologue as X

import Control.Applicative           as X
import Control.Lens                  as X
import Control.Monad                 as X (MonadPlus, mplus, mzero, void)
import Control.Monad.IO.Class        as X (MonadIO, liftIO)
import Control.Monad.Trans           as X (MonadTrans, lift)
import Data.Binary.Instances.Missing ()
import Data.Convert                  as X
--import           Data.Convertible.Instances.Missing as X
import Data.Default                   as X
import Data.Default.Instances.Missing ()
import Data.Foldable                  as X (Foldable, traverse_)
import Data.Foldable                  (forM_)
import Data.List                      (intersperse)
import Data.Monoid                    as X (Monoid, mappend, mconcat, mempty, (<>))
import Data.String.Class              as X (IsString (fromString), ToString (toString))
--import           Data.String.Repr                   as X (StrRepr, strRepr)
import           Control.Conditional as X (ifM, unless, unlessM, when, whenM)
import           Data.Maybe          as X (mapMaybe)
import           Data.Repr           as X
import           Data.Text.Class     as X (FromText (fromText), IsText, ToText (toText))
import           Data.Text.Lazy      as X (Text)
import qualified Data.Traversable    as Traversable
import           Data.Typeable       as X (Typeable)
import           GHC.Generics        as X (Generic)
import           Prelude             hiding (curry, mapM, mapM_, print, putStr, putStrLn, uncurry, (++), (.))
import qualified Prelude
