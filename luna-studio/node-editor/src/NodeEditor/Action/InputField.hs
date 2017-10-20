{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.InputField where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           JS.Atom                            (requestInputField)
import           LunaStudio.Data.PortRef            (dstPortId, nodeLoc)
import           NodeEditor.Action.State.Action     (beginActionWithKey, continueActionWithKey, removeActionFromState, updateActionWithKey)
import           NodeEditor.Action.State.App        (renderIfNeeded)
import           NodeEditor.Action.State.NodeEditor (getExpressionNode, modifyExpressionNode)
import           NodeEditor.React.Model.InputField  (ActiveInputField, InputFieldId (..), InputFieldMode (Single), content, fieldContent,
                                                     key, mkInputField, mode)
import           NodeEditor.React.Model.Node        (inPortAt)
import           NodeEditor.React.Model.Port        (state, _Constant, _TextValue, _WithDefault)
import           NodeEditor.State.Action            (Action (begin, continue, end, update), InputFieldActive (InputFieldActive),
                                                     inputFieldActiveAction, inputFieldActiveFieldId, inputFieldActiveState)
import           NodeEditor.State.Global            (State)




instance Action (Command State) InputFieldActive where
    begin    = beginActionWithKey    inputFieldActiveAction
    continue = continueActionWithKey inputFieldActiveAction
    update   = updateActionWithKey   inputFieldActiveAction
    end      = cancel


activateInputField :: InputFieldId -> Command State ()
activateInputField fid@(PortControlField portRef) = do
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. dstPortId
    modifyExpressionNode nl $ inPortAt pid . state . _WithDefault . _Constant . _TextValue . mode .= Single
    withJustM (getExpressionNode nl) $ \n ->
        withJust (n ^? inPortAt pid . state . _WithDefault . _Constant . _TextValue) $ \f -> do
            begin . InputFieldActive fid $ convert f
            renderIfNeeded
            requestInputField (key fid f) (f ^. content)

cancel :: InputFieldActive -> Command State ()
cancel action = case action ^. inputFieldActiveFieldId of
    PortControlField portRef -> do
        let nl  = portRef ^. nodeLoc
            pid = portRef ^. dstPortId
        withJustM (getExpressionNode nl) $ \n ->
            withJust (n ^? inPortAt pid . state . _WithDefault . _Constant . _TextValue) $ \f -> do
                print f
        modifyExpressionNode nl $ inPortAt pid . state . _WithDefault . _Constant . _TextValue . mode .= def
        removeActionFromState inputFieldActiveAction


accept :: InputFieldActive -> Command State ()
accept action = case action ^. inputFieldActiveFieldId of
    PortControlField portRef -> do
        let nl  = portRef ^. nodeLoc
            pid = portRef ^. dstPortId
        withJustM (getExpressionNode nl) $ \n ->
            withJust (n ^? inPortAt pid . state . _WithDefault . _Constant . _TextValue) $ \f -> do
                print f
        modifyExpressionNode nl $ inPortAt pid . state . _WithDefault . _Constant . _TextValue .= mkInputField (action ^. inputFieldActiveState . fieldContent)
        removeActionFromState inputFieldActiveAction


updateActiveInputField :: ActiveInputField -> InputFieldActive -> Command State ()
updateActiveInputField f action = update $ action & inputFieldActiveState .~ f


unfocus :: InputFieldActive -> Command State ()
unfocus action = case action ^. inputFieldActiveFieldId of
    PortControlField portRef -> accept action
