
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.InputField where

import           Common.Prelude
import qualified NodeEditor.Event.Keys             as Keys
import qualified NodeEditor.Event.UI               as UI
import qualified NodeEditor.React.Event.InputField as Field
import           NodeEditor.React.IsRef            (IsRef, dispatch)
import           NodeEditor.React.Model.InputField
import           React.Flux
import qualified React.Flux                        as React


import           System.IO.Unsafe                  (unsafePerformIO)

traceShowMToStdout :: (Show a, Monad m) => a -> m ()
traceShowMToStdout v = unsafePerformIO $ print v >> return (return ())

name :: JSString
name = "field"

inputField :: IsRef r => ReactView (r, InputFieldId, InputField)
inputField = React.defineView name $ \(ref, fid, model) -> let
    inactivePh = [ onClick $ \_ _ -> dispatch ref (UI.InputFieldEvent $ Field.ActivateInputField fid) ]
    singlePh = [ "id" $= key fid model
               , "className" $= "native-key-bindings"
               , onKeyDown   $ \e _ -> [stopPropagation e]
               , onMouseDown $ \e _ -> [stopPropagation e]
               ]
    in traceShowMToStdout model >> case model ^. mode of
        Inactive  -> div_ inactivePh . elemString . convert $ model ^. content
        Multiline -> textarea_ [] mempty
        Single    -> div_ singlePh mempty

inputField_ :: IsRef r => r -> InputFieldId -> InputField -> ReactElementM ViewEventHandler ()
inputField_ ref fid model = React.viewWithSKey inputField (key fid model) (ref, fid, model) mempty
