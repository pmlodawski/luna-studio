{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
module NodeEditor.Action.State.ResetNode where

import           Common.Prelude                             hiding (get)

import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import qualified NodeEditor.React.Model.Port                as Port
import qualified NodeEditor.React.Model.Visualization       as Visualization

import           Common.Action.Command                      (Command)
import           Data.Monoid                                (First (First), getFirst)
import           LunaStudio.Data.TypeRep                    (TypeRep (TStar))
import           NodeEditor.Action.State.NodeEditor         (getConnections, modifyExpressionNode)
import           NodeEditor.Action.State.Visualization      (setContent)
import           NodeEditor.React.Model.Connection          (dstNodeLoc, srcNodeLoc)
import           NodeEditor.React.Model.Node                (NodeLoc)
import           NodeEditor.State.Global                    (State)



resetSuccessors :: NodeLoc -> Command State ()
resetSuccessors nl = do
    outConnections <- filter (\c -> c ^. srcNodeLoc == nl) <$> getConnections
    let successors = view dstNodeLoc <$> outConnections
    whenM (resetNode nl) $ do
        mapM_ resetSuccessors successors

resetNode :: NodeLoc -> Command State Bool
resetNode nl = do
    maySuccess <- modifyExpressionNode nl $ do
        let resetPort = Port.valueType .~ TStar
        oldInPorts  <- use ExpressionNode.inPorts
        oldOutPorts <- use ExpressionNode.inPorts
        ExpressionNode.inPorts  %= fmap resetPort
        ExpressionNode.outPorts %= fmap resetPort
        ExpressionNode.value .= def
        newInPorts  <- use ExpressionNode.inPorts
        newOutPorts <- use ExpressionNode.inPorts
        return . First . Just
            $ (oldInPorts /= newInPorts) && (oldOutPorts /= newOutPorts)
    setContent
        nl
        (Visualization.Message Visualization.awaitingDataMsg)
    return $ fromMaybe False $ getFirst maySuccess
