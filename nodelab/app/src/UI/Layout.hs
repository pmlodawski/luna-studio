module UI.Layout where

import           Utils.PreludePlus
import           Utils.Vector
import           Control.Monad (forM, foldM_)

import           Object.UITypes
import           Object.Widget
import qualified Reactive.State.Global           as Global
import           Reactive.Commands.Command       (Command, performIO)

import           Reactive.State.UIRegistry     (sceneInterfaceId, sceneGraphId, addHandler)
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.Commands.UIRegistry  as UICmd

verticalLayout :: Double -> WidgetId -> Command UIRegistry.State ()
verticalLayout spacing id = UICmd.children id >>= mapM getHeight >>= foldM_ move 0.0 where
    getHeight id = do
        size <- UICmd.get' id widgetSize
        return $ (id, size ^. y)
    move offset (id, height) = do
        UICmd.moveY id offset
        return $ offset + spacing + height
