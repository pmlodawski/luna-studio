module JS.NodeSearcher where

import           Utils.PreludePlus

import           GHCJS.DOM.EventM
import           GHCJS.DOM.EventTargetClosures  (EventName, unsafeEventName)
import           GHCJS.DOM.Types                (IsUIEvent, UIEvent, Window, toUIEvent, unUIEvent)
import           GHCJS.Foreign
import           GHCJS.Types                    (JSRef, JSString)

import           Utils.Vector

import qualified Data.JSString                  as JSString
import           Data.JSString.Text             (lazyTextFromJSString, lazyTextToJSString)

import           JavaScript.Array               (JSArray)
import qualified JavaScript.Array               as JSArray

import           Data.Text.Lazy                 (Text)
import qualified Data.Text.Lazy                 as Text

import           Control.Monad.IO.Class         (MonadIO (..))


import           Text.ScopeSearcher.QueryResult (Highlight (..), QueryResult (..))



foreign import javascript safe "app.createNodeSearcher($1, $2, $3, $4, $5)"
    initNodeSearcher' :: JSString -> Int -> Int -> Int -> Bool -> IO ()

initNodeSearcher :: Text -> Int -> Vector2 Int -> Bool -> IO ()
initNodeSearcher expr nodeId pos = initNodeSearcher' (lazyTextToJSString expr) nodeId (pos ^. x) (pos ^. y)

foreign import javascript safe "app.destroyNodeSearcher()"
    destroyNodeSearcher :: IO ()

foreign import javascript safe "$2[\"detail\"][$1]"
    nodesearcher_event_get :: JSString -> JSRef UIEvent -> IO JSString

foreign import javascript safe "$1[\"detail\"][\"node\"]"
    nodesearcher_event_get_node :: JSRef UIEvent -> IO Int

nsEvent :: EventName Window UIEvent
nsEvent = (unsafeEventName (JSString.pack "ns_event"))

getKey :: (MonadIO m, IsUIEvent self) => String -> self -> m Text
getKey key self = liftIO $ do
    action <-  nodesearcher_event_get (JSString.pack key) (unUIEvent (toUIEvent self))
    return $ lazyTextFromJSString action

getExpression :: (MonadIO m, IsUIEvent self) => self -> m Text
getExpression = getKey "expression"

getAction :: (MonadIO m, IsUIEvent self) => self -> m Text
getAction     = getKey "action"


getNode :: (MonadIO m, IsUIEvent self) => self -> m Int
getNode self = liftIO $ do
    node <- nodesearcher_event_get_node (unUIEvent (toUIEvent self))
    return node


-- display results

data TargetSearcher = NodeSearcher | CommandSearcher

data JSHighlight

foreign import javascript safe "app.nodeSearcher().clearResults()"
    nodesearcher_clear_results :: IO ()

foreign import javascript safe "app.nodeSearcher().addResult($1, $2, $3, $4, $5)"
    nodesearcher_add_result :: JSString -> JSString -> JSString -> JSRef JSHighlight -> JSString -> IO ()

foreign import javascript safe "app.nodeSearcher().finishResult()"
    nodesearcher_finish_result :: IO ()

foreign import javascript safe "app.nodeSearcher().addTreeResult($1, $2, $3, $4)"
    nodesearcher_add_tree_result :: JSString -> JSString -> JSString -> JSString -> IO ()

searcherResultName :: TargetSearcher -> QueryResult -> Text
searcherResultName NodeSearcher    (QueryResult _ name     _ _ _ _) = name
searcherResultName CommandSearcher (QueryResult _ _ fullname _ _ _) = fullname

displayQueryResult :: TargetSearcher -> QueryResult -> IO ()
displayQueryResult target qr@(QueryResult prefix name fullname highlight tpe _) = do
    ary <- createJSArray
    mapM_ (pushHighlight ary) highlight
    let targetName = searcherResultName target qr
    nodesearcher_add_result (lazyTextToJSString prefix) (lazyTextToJSString name) (lazyTextToJSString targetName) ary (lazyTextToJSString tpe)
    nodesearcher_finish_result

displayQueryResults :: TargetSearcher -> [QueryResult] -> IO ()
displayQueryResults target results = do
    nodesearcher_clear_results
    forM_ results $ displayQueryResult target

displayTreeResult :: TargetSearcher -> QueryResult -> IO ()
displayTreeResult target qr@(QueryResult prefix name fullname _ tpe _) = do
    let targetName = searcherResultName target qr
    nodesearcher_add_tree_result (lazyTextToJSString prefix) (lazyTextToJSString name) (lazyTextToJSString targetName) (lazyTextToJSString tpe)

displayTreeResults :: TargetSearcher -> [QueryResult] -> IO ()
displayTreeResults target results = forM_ results $ displayTreeResult target

foreign import javascript safe "[]"
    createJSArray :: IO (JSRef JSHighlight)

foreign import javascript safe "$1.push({start: $2, length: $3})"
    pushHighlightJs :: JSRef JSHighlight -> Int -> Int -> IO (JSRef JSHighlight)

pushHighlight :: JSRef JSHighlight -> Highlight -> IO (JSRef JSHighlight)
pushHighlight acc (Highlight start len) = pushHighlightJs acc start len
