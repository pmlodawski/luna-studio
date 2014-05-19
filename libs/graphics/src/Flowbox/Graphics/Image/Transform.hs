---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Flowbox.Graphics.Image.Transform where

import qualified Data.Array.Accelerate as A
import qualified Data.Map              as Map

import           Flowbox.Prelude                hiding (index)
import           Flowbox.Graphics.Image         (Image)
import qualified Flowbox.Graphics.Image         as Image
import           Flowbox.Graphics.Image.Channel (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Shape         (RectangleAcc)



crop :: forall img ix a. (Image img (ChannelAcc ix a), A.Elt a, ix ~ A.DIM2, A.IsFloating a)
    => img (ChannelAcc ix a) -> RectangleAcc a -> a -> img (ChannelAcc A.DIM2 a)
crop image rect soft = image & Image.channels %~ resultImage
    where x1, y1, x2, y2 :: A.Exp a
          ((x1, y1), (x2, y2)) = rect

          leftCutoff, rightCutoff, topCutoff, lowCutoff :: A.Exp a
          leftCutoff  = (x1 A.<* x2) A.? (x1, x2)
          rightCutoff = (x1 A.<* x2) A.? (x2, x1)

          topCutoff = (y1 A.<* y2) A.? (y1, y2)
          lowCutoff = (y1 A.<* y2) A.? (y2, y1)

          resultHeight = A.round $ lowCutoff - topCutoff
          resultWidth  = A.round $ rightCutoff - leftCutoff
          resultShape  = A.index2 resultWidth resultHeight :: A.Exp A.DIM2

          translation :: A.Exp (Int, Int)
          translation = A.lift (A.round leftCutoff, A.round topCutoff)

          resultImage = Map.map (\chan ->
              Channel.generate resultShape (cut translation chan))

          cut :: A.Exp (Int, Int) -> ChannelAcc ix a -> A.Exp A.DIM2 -> A.Exp a
          cut translationVector chan sh = chan Channel.! (A.lift (A.Z A.:. x + xDiff A.:. y + yDiff))
              where (A.Z A.:. x A.:. y) = A.unlift sh :: A.Z A.:. A.Exp Int A.:. A.Exp Int
                    (xDiff, yDiff)      = A.unlift translationVector :: (A.Exp Int, A.Exp Int)


softness :: forall img ix a. (Image img (ChannelAcc ix a), A.Elt a, ix ~ A.DIM2, A.IsFloating a)
    => img (ChannelAcc ix a) -> A.Exp a -> A.Exp a -> img (ChannelAcc ix a)
softness image width height = image & Image.channels %~ resultImage
    where resultImage = Map.map (\chan -> Channel.zipWith (*) chan mask)

          mask = Channel.generate sourceShape (\index ->
              let A.Z A.:. y' A.:. x' = A.unlift index :: A.Z A.:. A.Exp Int A.:. A.Exp Int
                  y = A.fromIntegral y' :: A.Exp a
                  x = A.fromIntegral x' :: A.Exp a
              in y A.>* height A.? (
                  y A.>* (chanHeight' - height) A.? (
                      x A.>* width A.? (
                          x A.>* (chanWidth' - width) A.? (
                              -- right top
                              let ellipseDistance =
                                      sqrt(((y - (chanHeight' - height)) / height)**2 + ((x - (chanWidth' - width)) / width)**2)
                                  clamped = ellipseDistance A.>* 1.0 A.? (1.0, ellipseDistance)
                              in 1 - clamped
                              ,
                              -- center top
                              let distance = (y - (chanHeight' - height)) / height
                              in 1 - distance
                              )
                          ,
                          -- left top
                          let ellipseDistance = sqrt(((y - (chanHeight' - height)) / height)**2 + ((x - width) / width)**2)
                              clamped = ellipseDistance A.>* 1.0 A.? (1.0, ellipseDistance)
                          in 1 - clamped
                          )
                      ,
                      x A.>* width A.? (
                          x A.>* (chanWidth' - width) A.? (
                              -- right center
                              let distance = (x - (chanWidth' - width)) / width
                              in 1 - distance
                              ,
                              -- center center
                              1
                              )
                          ,
                          -- left center
                          let distance = x / width
                          in distance
                          )
                      )
                  ,
                  x A.>* width A.? (
                      x A.>* (chanWidth' - width) A.? (
                          -- right bottom
                          let ellipseDistance = sqrt(((y - height) / height)**2 + ((x - (chanWidth' - width)) / width)**2)
                              clamped = ellipseDistance A.>* 1.0 A.? (1.0, ellipseDistance)
                          in 1 - clamped
                          ,
                          -- center bottom
                          let distance = y / height
                          in distance
                          )
                      ,
                      -- left bottom
                      let ellipseDistance = sqrt(((y - height) / height)**2 + ((x - width) / width)**2)
                          clamped = ellipseDistance A.>* 1.0 A.? (1.0, ellipseDistance)
                      in 1 - clamped
                      )
                  ))

          sourceShape = image ^. Image.channels . to Map.elems . to head . to Channel.shape
          A.Z A.:. chanWidth A.:. chanHeight = A.unlift $ sourceShape :: A.Z A.:. A.Exp Int A.:. A.Exp Int
          chanWidth'  = A.fromIntegral chanWidth  :: A.Exp a
          chanHeight' = A.fromIntegral chanHeight :: A.Exp a

          --leftCutoffCoefficientMatrix = Channel.generate sourceShape (\index ->
          --    let A.Z A.:. _ A.:. x = A.unlift index :: A.Z A.:. A.Exp Int A.:. A.Exp Int
          --    in A.fromIntegral x / width)
          --rightCutoffCoefficientMatrix = Channel.generate sourceShape (\index ->
          --    let A.Z A.:. _ A.:.x = A.unlift index :: A.Z A.:. A.Exp Int A.:. A.Exp Int
          --    in A.fromIntegral (chanWidth - x) / width)
          --topCutoffCoefficientMatrix = Channel.generate sourceShape (\index ->
          --    let A.Z A.:. y A.:. _ = A.unlift index :: A.Z A.:. A.Exp Int A.:. A.Exp Int
          --    in A.fromIntegral (chanHeight - y) / height)
          --lowCutoffCoefficientMatrix = Channel.generate sourceShape (\index ->
          --    let A.Z A.:. y A.:. _ = A.unlift index :: A.Z A.:. A.Exp Int A.:. A.Exp Int
          --    in A.fromIntegral y / height)
  
          --preNormalizedCoefficientMatrix = Channel.generate sourceShape (\index ->
          --    let left  = leftCutoffCoefficientMatrix Channel.! index
          --        right = rightCutoffCoefficientMatrix Channel.! index
          --        top   = topCutoffCoefficientMatrix Channel.! index
          --        low   = lowCutoffCoefficientMatrix Channel.! index
          --        dist  = distanceFromCenterMatrix Channel.! index
          --        --dist  = distanceAsEllipse Channel.! index
          --        minimalCoeff = min left $ min right $ min top $ min low dist
          --    in minimalCoeff)
  
          --normalizedCoefficientMatrix = Channel.generate sourceShape (\index ->
          --    let one              = A.constant (1.0 :: a)
          --        inputCoefficient = preNormalizedCoefficientMatrix Channel.! index
          --        finalCoefficient = inputCoefficient A.>* one A.? (one, inputCoefficient)
          --    in finalCoefficient)
         
          --maxPreNormalized = A.the $ A.maximum $ Channel.accMatrix preNormalizedCoefficientMatrix
          --normalizedCoefficientMatrix = Channel.generate sourceShape (\index ->
          --    let pix = preNormalizedCoefficientMatrix Channel.! index
          --        result = pix / maxPreNormalized
          --    in result)

          --distanceFromCenterHorizMatrix = Channel.generate sourceShape (\index ->
          --    let A.Z A.:. _ A.:. x = A.unlift index :: A.Z A.:. A.Exp Int A.:. A.Exp Int
          --        diff = abs ((A.fromIntegral x - (chanWidth' / 2)) / chanWidth' / 2)
          --    in diff)

          --distanceFromCenterVertMatrix = Channel.generate sourceShape (\index ->
          --    let A.Z A.:. y A.:. _ = A.unlift index :: A.Z A.:. A.Exp Int A.:. A.Exp Int
          --        diff = abs ((A.fromIntegral y - (chanHeight' / 2)) / chanHeight' / 2)
          --    in diff)

          --preDistanceAsEllipse = Channel.generate sourceShape (\index ->
          --    let A.Z A.:. y A.:. x = A.unlift index :: A.Z A.:. A.Exp Int A.:. A.Exp Int
          --        y' = A.fromIntegral $ y + 1
          --        x' = A.fromIntegral $ x + 1
          --        a  = A.constant (30 :: a)
          --        b  = A.constant (30 :: a)
          --        halfHeight = chanHeight' / 2 :: A.Exp a
          --        halfWidth  = chanWidth' / 2  :: A.Exp a
          --        xCoeff = abs (x' - halfWidth) / a  :: A.Exp a
          --        yCoeff = abs (y' - halfHeight) / b :: A.Exp a
          --        diff = sqrt(yCoeff ** 2 + xCoeff ** 2) :: A.Exp a
          --    in diff)

          --preDistanceMax = A.the $ A.maximum $ Channel.accMatrix preDistanceAsEllipse
          --distanceAsEllipse = Channel.generate sourceShape (\index ->
          --    let pix = preDistanceAsEllipse Channel.! index
          --        result = pix / preDistanceMax
          --    in 1 - result)

          --distanceAsEllipse = Channel.generate sourceShape (\index ->
          --  let one              = A.constant (1.0 :: a)
          --      inputCoefficient = preDistanceAsEllipse Channel.! index
          --      finalCoefficient = inputCoefficient A.>* one A.? (one, inputCoefficient)
          --  in finalCoefficient)

          --distanceFromCenterMatrix = Channel.zipWith (\a b -> let foo = sqrt(a ** 2 + b ** 2) in 1-foo) distanceFromCenterHorizMatrix distanceFromCenterVertMatrix
