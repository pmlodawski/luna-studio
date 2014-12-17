---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Flowbox.Graphics.Color.Conversion where

import Data.Array.Accelerate



class ColorConvert a b where
    convertColor :: (Elt t, IsFloating t) => a (Exp t) -> b (Exp t)

liftedConvertColor :: forall a b t. (
                      ColorConvert a b, Elt t, IsFloating t,
                      Plain (b (Exp t)) ~ b t, Plain (a (Exp t)) ~ a t,
                      Lift Exp (b (Exp t)), Unlift Exp (a (Exp t))
                      )
                   => Exp (a t)
                   -> Exp (b t)
liftedConvertColor = lift1 cc
    where cc :: a (Exp t) -> b (Exp t)
          cc = convertColor
