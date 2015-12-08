{-# LANGUAGE OverloadedStrings #-}

module UI.Handlers.List where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.Widget
import           Object.UITypes
import qualified Data.Text.Lazy as Text

import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.UIRegistry    as UIRegistry
import qualified Reactive.State.Global        as Global
import           Reactive.State.Global (inRegistry)
import           Reactive.Commands.Command (Command)

import           Data.HMap.Lazy (HTMap)
import           Data.HMap.Lazy (TypeKey(..))

import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)
import           Object.Widget.List  (List(..))
import           Reactive.State.UIRegistry (addHandler)
import           UI.Generic (takeFocus, startDrag)
import           UI.Layout as Layout
import           UI.Widget.List ()
import           UI.Widget.Toggle ()
import qualified Data.HMap.Lazy as HMap
import qualified Object.Widget.Button  as Button
import qualified Object.Widget.Group   as Group
import qualified Object.Widget.Label   as Label
import qualified Object.Widget.List    as List
import qualified Object.Widget.List    as List
import qualified Object.Widget.TextBox as TextBox
import qualified UI.Handlers.Button    as Button
import qualified UI.Handlers.TextBox   as TextBox
import           Object.LunaValue
import           UI.Widget.List.Constructor ()


