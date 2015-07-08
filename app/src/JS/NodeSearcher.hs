module JS.NodeSearcher where

import Control.Monad.Trans ( liftIO )

import GHCJS.Foreign
import GHCJS.DOM.EventM
import GHCJS.Types        ( JSRef, JSArray, JSString )
import GHCJS.DOM.Types    ( UIEvent, IsDOMWindow, IsUIEvent, unUIEvent, toUIEvent )

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)


data NodeSearcher

foreign import javascript unsafe "app.createNodeSearcher($1, $2, $3)"
    initNodeSearcher :: JSString -> Int -> Int-> IO ()

foreign import javascript unsafe "app.destroyNodeSearcher()"
    destroyNodeSearcher :: IO ()

nodeSearcherOnEvent :: (IsDOMWindow self) => Signal self (EventM UIEvent self ())
nodeSearcherOnEvent = (connect ("ns_event" :: [Char]))


foreign import javascript unsafe "$1[\"detail\"][\"expression\"]"
    nodesearcher_event_get_expression :: JSRef UIEvent -> IO JSString

nodeSearcherEventGetExpression :: (IsUIEvent self) => self -> IO Text
nodeSearcherEventGetExpression self = do
    expression <- nodesearcher_event_get_expression (unUIEvent (toUIEvent self))
    return $ fromJSString expression

nsExpression :: IsUIEvent e => EventM e t Text
nsExpression = event >>= (liftIO . nodeSearcherEventGetExpression)


foreign import javascript unsafe "$1[\"detail\"][\"action\"]"
    nodesearcher_event_get_action :: JSRef UIEvent -> IO JSString

nodeSearcherEventGetAction :: (IsUIEvent self) => self -> IO Text
nodeSearcherEventGetAction self = do
    action <- nodesearcher_event_get_action (unUIEvent (toUIEvent self))
    return $ fromJSString action

nsAction :: IsUIEvent e => EventM e t Text
nsAction = event >>= (liftIO . nodeSearcherEventGetAction)



-- display results

foreign import javascript unsafe "app.nodeSearcher().clearResults()"
    nodesearcher_clear_results :: IO ()

foreign import javascript unsafe "app.nodeSearcher().addResult($1, $2, $3, [], $4)"
    nodesearcher_add_result :: JSString -> JSString -> JSString -> JSString -> IO ()

data Highlight = Highlight {start :: Int, len :: Int} deriving (Show, Eq)
data QueryResult = QueryResult {_prefix :: Text, _name :: Text, _fullname :: Text, _highlights :: [Highlight], _tpe :: Text}

displayQueryResult :: QueryResult -> IO ()
displayQueryResult (QueryResult prefix name fullname _ tpe ) = nodesearcher_add_result (toJSString prefix) (toJSString name) (toJSString fullname) (toJSString tpe)

displayQueryResults :: [QueryResult] -> IO ()
displayQueryResults results = do
    nodesearcher_clear_results
    mapM_ displayQueryResult results

--
-- nodeSearcherEventGetAction :: (IsUIEvent self) => self -> IO Text
-- nodeSearcherEventGetAction self = do
--     action <- nodesearcher_event_get_action (unUIEvent (toUIEvent self))
--     return $ fromJSString action
--
-- nsAction :: IsUIEvent e => EventM e t Text
-- nsAction = event >>= (liftIO . nodeSearcherEventGetAction)