module NodeEditor.Action.State.App where

import           Common.Prelude hiding (get)

import           Control.Lens.Internal.Zoom (Focusing)
import qualified Control.Monad.State        as M

import           Common.Action.Command              (Command)
import qualified NodeEditor.State.UI                as UI
import           NodeEditor.State.Global            (State, ui)
import           NodeEditor.React.Model.App         (App, breadcrumbs, workspace)
import           NodeEditor.Batch.Workspace         (Workspace)
import           NodeEditor.View.App                (appView)
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumb, BreadcrumbItem, Named)


renderIfNeeded :: Command State ()
renderIfNeeded = timeIt "render" $ do
    newUI <- UI.renderIfNeeded appView =<< use ui
    ui .= newUI

modifyApp :: M.State App r -> Command State r
modifyApp action = do
    (result, newUI) <- UI.modify action <$> use ui
    ui .= newUI
    return result

modify :: LensLike' (Focusing Identity b) App s -> M.State s b -> Command State b
modify lens action = modifyApp $ zoom lens action

get :: Getting r App r -> Command State r
get lens = use $ ui . UI.app . lens

setBreadcrumbs :: Breadcrumb (Named BreadcrumbItem) -> Command State ()
setBreadcrumbs bc = modifyApp $ breadcrumbs .= bc

getWorkspace :: Command State (Maybe Workspace)
getWorkspace = get workspace
