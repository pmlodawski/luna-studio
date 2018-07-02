{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Empire.Data.Layers (
    Marker
  , Meta
  , TypeLayer
  , SpanLength
  , SpanOffset
  , attachEmpireLayers
  ) where

import Empire.Prelude

import           Control.Lens.Iso         (from)
import           LunaStudio.Data.Node     (NodeId)
import           LunaStudio.Data.NodeMeta (NodeMetaS)
import           LunaStudio.Data.PortRef  (OutPortRefS)

-- import Data.RefMask.Mutable (Encoder1(..))
import           Control.Monad.Exception        (Throws)
import           Luna.IR                        hiding (Import, Marker, String)
-- import qualified Luna.IR.Layer.Type             as IR (Type)
-- import           Luna.IR.ToRefactor2
-- import           Luna.Syntax.Text.Parser.State.Marker (MarkedExprMap)
-- import           OCI.IR.Class                   (Import)
import           Luna.Pass
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan
-- import           Data.Graph.Component.Provider   (DynamicProvider1)
import           Data.Graph.Data.Layer.Class   (Layer)
import qualified Data.Graph.Component.Node.Layer   as Layer
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.Graph.Data.Layer.Layout                as Layout
-- import           OCI.Pass.Definition            (makePass)
import           Type.Any
import           Data.Text.Position             (Delta(..))
import qualified Prologue                       as Prologue (mempty)


type TypeLayer = Layer.Type

-- instance DynamicProvider1 Maybe

data Marker
instance Layer Marker where
    type Cons Marker = Layer.Simple (Maybe OutPortRefS)
    type Layout Marker layout = Layout.Get Marker layout
    manager = Layer.staticManager

data Meta
instance Layer Meta where
    type Cons Meta = Layer.Simple (Maybe NodeMetaS)
    type Layout Meta layout = Layout.Get Meta layout
    manager = Layer.staticManager

data SpanOffset
instance Layer SpanOffset where
    type Cons SpanOffset = Layer.Simple Delta
    type Layout SpanOffset layout = Layout.Get SpanOffset layout
    manager = Layer.staticManager

data SpanLength
instance Layer SpanLength where
    type Cons SpanLength = Layer.Simple Delta
    type Layout SpanLength layout = Layout.Get SpanLength layout
    manager = Layer.staticManager

instance Default a => Default1 (Layer.Simple a) where
    def1 = wrap def

attachEmpireLayers :: _ => m ()
attachEmpireLayers = return ()
    -- attachLayer @Meta       @AnyExpr
    -- attachLayer @Marker     @AnyExpr
    -- attachLayer @SpanLength @AnyExpr
    -- attachLayer @SpanOffset @AnyExprLink
    -- attachLayer @CodeSpan.CodeSpan   @AnyExpr
