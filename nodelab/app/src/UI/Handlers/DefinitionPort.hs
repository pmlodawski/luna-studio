module UI.Handlers.DefinitionPort where

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
        let inOut = model ^. Model.inputOutput
            tx = Definition.portLabelGap + Definition.portHoverWidth
            lx = model ^. Model.size . x - tx
            ly = model ^. Model.size . y
            unalignedLabel = Label.create (Vector2 lx ly) (model ^. Model.labelValue)
            label = case inOut of
                Model.Input -> unalignedLabel
                Model.Output -> unalignedLabel & Label.alignment .~ Label.Right
        labelId <- UICmd.register wid label def
        case model ^. Model.inputOutput of
            Model.Input  -> UICmd.moveX labelId tx
            Model.Output -> do
                UICmd.moveX wid lx
                UICmd.moveX labelId $ -lx - Definition.portLabelGap

    updateWidget wid old model = do
        (labelId:_) <- UICmd.children wid
        UICmd.update_ labelId $ Label.label .~ (model ^. Model.labelValue)


instance ResizableWidget Model.DefinitionPort where resizeWidget = UI.defaultResize


widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & mouseOver .~ mouseOverHandler
                     & mouseOut  .~ mouseOutHandler


mouseOverHandler :: MouseOverHandler Global.State
mouseOverHandler _ wid =
    Global.inRegistry $ UICmd.update_ wid $ Model.hovered .~ True

mouseOutHandler :: MouseOutHandler Global.State
mouseOutHandler _ wid =
    Global.inRegistry $ UICmd.update_ wid $ Model.hovered .~ False
