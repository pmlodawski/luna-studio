module Empire.Commands.Graph.Context
    ( module Empire.Commands.Graph.Context
    , withLibrary
    ) where


import Empire.Prelude

import qualified Data.Graph.Store              as Store
import qualified Empire.Commands.Publisher     as Publisher
import qualified LunaStudio.Data.GraphLocation as GraphLocation

import Empire.Commands.Graph.Breadcrumb (zoomBreadcrumb)
import Empire.Commands.Library          (withLibrary)
import Empire.ASTOp                     (runASTOp)
import Empire.Data.AST                  (astExceptionFromException,
                                         astExceptionToException)
import Empire.Data.Graph                (ClsGraph, Graph, clsClass, userState)
import Empire.Empire                    (Command, Empire, activeInterpreter)
import LunaStudio.Data.GraphLocation    (GraphLocation)


data UnsupportedOperation = UnsupportedOperation deriving Show

instance Exception UnsupportedOperation where
    fromException = astExceptionFromException
    toException   = astExceptionToException

withGraph :: GraphLocation -> Command Graph a -> Empire a
withGraph gl act = withBreadcrumb gl act (throwM UnsupportedOperation)

withUnit :: GraphLocation -> Command ClsGraph a -> Empire a
withUnit gl act = withBreadcrumb gl (throwM UnsupportedOperation) act

withBreadcrumb
    :: GraphLocation -> Command Graph a -> Command ClsGraph a -> Empire a
withBreadcrumb gl = withLibrary (gl ^. GraphLocation.filePath)
    .: zoomBreadcrumb (gl ^. GraphLocation.breadcrumb)

withTC' :: GraphLocation -> Bool -> Command Graph a -> Command ClsGraph a -> Empire a
withTC' loc flush actG actC = do
    res       <- withBreadcrumb loc actG actC
    interpret <- use $ userState . activeInterpreter
    withBreadcrumb (GraphLocation.top loc) (return ()) (runTC loc flush interpret False)
    return res

withTCUnit :: GraphLocation -> Bool -> Command ClsGraph a -> Empire a
withTCUnit loc flush cmd = withTC' loc flush (throwM UnsupportedOperation) cmd

withTC :: GraphLocation -> Bool -> Command Graph a -> Empire a
withTC loc flush actG = withTC' loc flush actG (throwM UnsupportedOperation)

typecheck :: GraphLocation -> Empire ()
typecheck loc = withTC' loc False (return ()) (return ())

runTC :: GraphLocation -> Bool -> Bool -> Bool -> Command ClsGraph ()
runTC loc flush interpret recompute = do
    g <- use userState
    let root = g ^. clsClass
    rooted <- runASTOp $ Store.serializeWithRedirectMap root
    Publisher.requestTC loc g rooted flush interpret recompute

typecheckWithRecompute :: GraphLocation -> Empire ()
typecheckWithRecompute loc = do
    withBreadcrumb (GraphLocation.top loc) (return ()) (runTC loc True True True)

runInterpreter :: GraphLocation -> Empire ()
runInterpreter loc = do
    withBreadcrumb (GraphLocation.top loc) (return ()) (runTC loc True True False)
