{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.PortControl
    ( portControl_
    ) where

import           Common.Prelude                    hiding (group)
import qualified JS.Mount                          as Mount
import           LunaStudio.Data.Port              (InPortIndex (Arg))
import           LunaStudio.Data.PortRef           (InPortRef (InPortRef))
import           LunaStudio.Data.TypeRep           (TypeRep (TCons))
import           NodeEditor.Data.Slider            (InitValue (Continous, Discrete))
import qualified NodeEditor.Event.UI               as UI
import qualified NodeEditor.React.Event.Node       as Node
import           NodeEditor.React.IsRef            (IsRef, dispatch)
import           NodeEditor.React.Model.InputField (InputFieldId (PortControlField))
import           NodeEditor.React.Model.Node       (NodeLoc)
import           NodeEditor.React.Model.Port       (InPort, PortDefault (Constant), PortState (..), PortValue (..), _BoolValue, _Constant,
                                                    _IntValue, _RealValue, _TextValue)
import qualified NodeEditor.React.Model.Port       as Port
import           NodeEditor.React.View.InputField  (inputField_)
import qualified NodeEditor.React.View.Style       as Style
import           React.Flux                        as React


roundTo :: Int -> Double -> Double
roundTo limit a =  round' (f * a) / f
  where
    round' v = fromIntegral (round v :: Integer)
    f        = fromIntegral (10^limit :: Integer)

integer :: String -> String
integer a = if null a then "" else case last a of
    '.' -> init a
    _   -> integer $ init a

fractional :: String -> String
fractional a = case a of
    []     -> ""
    '.':xs -> take 3 xs
    _:xs   -> fractional xs

fractionalExp :: String -> String
fractionalExp a = case a of
        []     -> ""
        '.':xs -> take 3 xs <> "…" <> ePart xs
        _:xs   -> fractionalExp xs
    where
        ePart b =
            case b of
                []     -> ""
                'e':bs -> 'e':bs
                _:bs   -> ePart bs

isExpNotation :: String -> Bool
isExpNotation a =
    case a of
        []    -> False
        'e':_ -> True
        _:xs  -> isExpNotation xs

portControlId, labelPrefix, controlPrefix :: JSString
portControlId = Mount.prefix "focus-portcontrol"
labelPrefix   = Mount.prefix "label-"
controlPrefix = Mount.prefix "control-"

portControl_ :: IsRef r => r -> NodeLoc -> InPort -> ReactElementM ViewEventHandler ()
portControl_ ref' nl' port' = React.viewWithSKey portControl (jsShow $ port' ^. Port.portId) (ref', nl', port') mempty

portControl :: IsRef r => ReactView (r, NodeLoc, InPort)
portControl = React.defineView "portControl" $ \(ref, nl, port) ->

    case port ^. Port.portId of
        [Arg _] -> row ["node__control"] $ do
                        div_
                           [ "key"       $= (labelPrefix <> jsShow (port ^. Port.portId))
                           , "className" $= Style.prefix "node__label"
                           ] $ elemString . convert $ port ^. Port.name
                        portCtrl ref nl port False

        []      -> row ["node__control", "node__control--alias"] $ do
                        div_
                           [ "key"       $= "alias-label"
                           , "className" $= Style.prefix "node__label"
                           ] $ elemString "alias"
                        portCtrl ref nl port True
        _       -> row ["node__control", "node__control--self"] $ do
                        div_
                           [ "key"       $= "self-label"
                           , "className" $= Style.prefix "node__label"
                           ] $ elemString "self"
                        return ()

    where
        row classList = div_ [ "className" $= Style.prefixFromList classList ]

        portCtrl ref nl port isAlias = do
            let valueClass  = case port ^. Port.state of
                    NotConnected  -> "node__ctrl--not-connected"
                    Connected     -> "node__ctrl--connected"
                    WithDefault _ -> "node__ctrl--with-default"
                portRef     = InPortRef nl $ port ^. Port.portId
            div_
                [ "key"       $= (controlPrefix <> jsShow (port ^. Port.portId))
                , "className" $= Style.prefixFromList [ "node__ctrl", valueClass, if isAlias then "node__ctrl--alias" else "" ]
                ] $ case port ^. Port.state of
                    NotConnected -> do
                        let tConsElem :: PortValue -> ReactElementM ViewEventHandler ()
                            tConsElem zeroValue = button_
                                [ "className" $= Style.prefix "ctrl--set"
                                , onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetPortDefault portRef $ Constant zeroValue
                                ] $ elemString "set"
                        case port ^. Port.valueType of
                            TCons "Int"  _ -> tConsElem $ IntValue  def
                            TCons "Real" _ -> tConsElem $ RealValue def
                            TCons "Text" _ -> tConsElem $ TextValue def
                            TCons "Bool" _ -> tConsElem $ BoolValue False
                            _              -> elemString ""
                    Connected -> elemString "" -- TODO get current value from the connected port
                    WithDefault defVal -> void $ case port ^. Port.valueType of
                        TCons "Int" _ -> do
                            let value       = fromMaybe 0 $ defVal ^? _Constant . _IntValue
                            div_
                                [ "className" $= Style.prefixFromList [ "ctrl--slider", "ctrl--slider--int" ]
                                --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                                , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Discrete value)
                                ] $ elemString $ show value
                        TCons "Real" _ -> do
                            let value = fromMaybe 0.0 $ defVal ^? _Constant . _RealValue
                            div_
                                [ "className" $= Style.prefixFromList [ "ctrl--slider", "ctrl--slider--real" ]
                                --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                                , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Continous value)
                                ] $ do
                                    let val = show $ roundTo 3 value
                                    div_ [ "className" $= Style.prefix "ctrl--slider__left"  ] $ elemString $ integer val
                                    div_ [ "className" $= Style.prefix "ctrl--slider__right" ] $ elemString $ (if isExpNotation val then fractionalExp else fractional) val
                        TCons "Text" _ -> do
                            let mayField = defVal ^? _Constant . _TextValue
                            withJust mayField $ inputField_ ref (PortControlField portRef)
                        TCons "Bool" _ -> do
                            let value = fromMaybe True $ defVal ^? _Constant . _BoolValue
                                defaultValue = Constant $ BoolValue $ not value
                            div_
                                [ onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetPortDefault portRef defaultValue
                                , "className" $= Style.prefix ("ctrl--bool--" <> jsShow value)
                                ] mempty
                        _ -> elemString ""


