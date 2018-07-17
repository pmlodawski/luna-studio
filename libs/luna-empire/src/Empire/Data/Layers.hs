{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Empire.Data.Layers (
    TypeLayer
  , Marker
  , Meta
  , SpanLength
  , SpanOffset
  ) where

import Empire.Prelude

import qualified Data.Graph.Component.Node.Layer  as Layer
import qualified Data.Graph.Data.Layer.Class      as Layer
import Data.Graph.Component.Node.Layer.PortMarker (PortMarker)
import Data.Graph.Component.Node.Layer.NodeMeta   (Meta)
import Data.Graph.Component.Node.Layer.SpanLength (SpanLength)
import Data.Graph.Component.Node.Layer.SpanOffset (SpanOffset)

type TypeLayer = Layer.Type
type Marker = PortMarker
