module UI.Handlers.DefinitionPort where

import           Object.Widget                (UIHandlers)
import           Object.Widget
import qualified Object.Widget.DefinitionPort as Model
import qualified Object.Widget.Label          as Label
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global        as Global
import qualified Style.Definition             as Definition
import qualified UI.Generic                   as UI
import qualified UI.Registry                  as UI
import           UI.Widget.DefinitionPort     ()
import           UI.Widget.Label              ()
import           UI.Widget.Toggle             ()
import           Utils.PreludePlus
import           Utils.Vector                 (Vector2 (Vector2), x, y)



instance CompositeWidget Model.DefinitionPort where
    createWidget wid model = do
        let tx = Definition.portLabelGap + Definition.portHoverWidth
            lx = model ^. Model.size . x - tx
            ly = model ^. Model.size . y
            label = Label.create (Vector2 lx ly) (model ^. Model.labelValue)
        labelId <- UICmd.register wid label def
        UICmd.moveX labelId tx

    updateWidget wid old model = do
        (labelId:_) <- UICmd.children wid
        UICmd.update_ labelId $ Label.label .~ (model ^. Model.labelValue)


instance ResizableWidget Model.DefinitionPort where resizeWidget = UI.defaultResize


widgetHandlers :: UIHandlers Global.State
widgetHandlers = def
