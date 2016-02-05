---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Syntax.Graph.Port where

import           Flowbox.Prelude



data Port = All
          | Num Int
          deriving (Eq, Show, Ord, Read)


newtype DstPortP port = DstPort { _dstPort :: port } deriving (Eq, Show, Ord, Read)
newtype SrcPortP port = SrcPort { _srcPort :: port } deriving (Eq, Show, Ord, Read)

type DstPort = DstPortP Port
type SrcPort = SrcPortP Port

makeLenses ''DstPortP
makeLenses ''SrcPortP


toList :: Port -> [Int]
toList All     = []
toList (Num a) = [a]


instance Wrap   DstPortP where wrap = DstPort
instance Wrap   SrcPortP where wrap = SrcPort
instance Unwrap DstPortP where unwrap = view dstPort
instance Unwrap SrcPortP where unwrap = view srcPort


mkDst :: Int -> DstPort
mkDst = DstPort . Num

mkDstAll :: DstPort
mkDstAll = DstPort All

mkSrc :: Int -> SrcPort
mkSrc = SrcPort . Num

mkSrcAll :: SrcPort
mkSrcAll = SrcPort All
