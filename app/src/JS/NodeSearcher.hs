module JS.NodeSearcher where

import Data.Foldable ( foldlM )

import Control.Monad.Trans ( liftIO )

import GHCJS.Foreign
import GHCJS.DOM.EventM
import GHCJS.Types        ( JSRef, JSArray, JSString )
import GHCJS.DOM.Types    ( UIEvent, IsDOMWindow, IsUIEvent, unUIEvent, toUIEvent )

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

foreign import javascript unsafe "app.createNodeSearcher($1, $2, $3)"
    initNodeSearcher :: JSString -> Int -> Int-> IO ()

foreign import javascript unsafe "app.destroyNodeSearcher()"
    destroyNodeSearcher :: IO ()

nodeSearcherOnEvent :: (IsDOMWindow self) => Signal self (EventM UIEvent self ())
nodeSearcherOnEvent = (connect ("ns_event" :: [Char]))


foreign import javascript unsafe "$2[\"detail\"][$1]"
    nodesearcher_event_get :: JSString -> JSRef UIEvent -> IO JSString

unwrapJSString :: IsUIEvent e => (JSRef UIEvent -> IO JSString) -> EventM e t Text
unwrapJSString getFunction = event >>= (liftIO . nodeSearcherEventGet getFunction)

nsExpression :: IsUIEvent e => EventM e t Text
nsExpression = unwrapJSString . nodesearcher_event_get $ toJSString "expression"

nsAction :: IsUIEvent e => EventM e t Text
nsAction = unwrapJSString . nodesearcher_event_get $ toJSString "action"

nodeSearcherEventGet :: (IsUIEvent self) => (JSRef UIEvent -> IO JSString) -> self -> IO Text
nodeSearcherEventGet getFunction self = do
    result <- getFunction (unUIEvent (toUIEvent self))
    return $ fromJSString result

-- display results

data JSHighlight

foreign import javascript unsafe "app.nodeSearcher().clearResults()"
    nodesearcher_clear_results :: IO ()

foreign import javascript unsafe "app.nodeSearcher().addResult($1, $2, $3, $4, $5)"
    nodesearcher_add_result :: JSString -> JSString -> JSString -> JSArray JSHighlight -> JSString -> IO ()

foreign import javascript unsafe "app.nodeSearcher().addTreeResult($1, $2, $3, $4)"
    nodesearcher_add_tree_result :: JSString -> JSString -> JSString -> JSString -> IO ()

data Highlight = Highlight {start :: Int, len :: Int} deriving (Show, Eq)
data QueryResult = QueryResult {_prefix :: Text, _name :: Text, _fullname :: Text, _highlights :: [Highlight], _tpe :: Text}

displayQueryResult :: QueryResult -> IO ()
displayQueryResult (QueryResult prefix name fullname highlight tpe) = do
    ary <- createJSArray
    mapM_ (pushHighlight ary) highlight
    nodesearcher_add_result (toJSString prefix) (toJSString name) (toJSString fullname) ary (toJSString tpe)

displayQueryResults :: [QueryResult] -> IO ()
displayQueryResults results = do
    nodesearcher_clear_results
    mapM_ displayQueryResult results

displayTreeResult :: QueryResult -> IO ()
displayTreeResult (QueryResult prefix name fullname _ tpe ) = do
    nodesearcher_add_tree_result (toJSString prefix) (toJSString name) (toJSString fullname) (toJSString tpe)

displayTreeResults :: [QueryResult] -> IO ()
displayTreeResults results = mapM_ displayTreeResult results

foreign import javascript unsafe "[]"
    createJSArray :: IO (JSArray JSHighlight)

foreign import javascript unsafe "$1.push({start: $2, length: $3})"
    pushHighlightJs :: JSArray JSHighlight -> Int -> Int -> IO (JSArray JSHighlight)

pushHighlight :: JSArray JSHighlight -> Highlight -> IO (JSArray JSHighlight)
pushHighlight acc (Highlight start len) = pushHighlightJs acc start len
