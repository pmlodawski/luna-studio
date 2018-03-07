{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.View.Searcher where

import           Common.Data.JSON                (toJSONVal)
import           Common.Prelude
import           Data.Aeson                      (ToJSON)
import           Data.Convert                    (Convertible (convert))
import           NodeEditor.React.Model.Searcher (Searcher)
import qualified NodeEditor.React.Model.Searcher as Searcher
import           LunaStudio.Data.NodeSearcher    (Match)
import qualified LunaStudio.Data.NodeSearcher    as Match



searcherView :: MonadIO m => Maybe Searcher -> Maybe Searcher -> m ()
searcherView new old =
    when (new /= old) $ setSearcher $ convert new

data EntryView = EntryView
    { name :: String
    , doc  :: String
    , tpe  :: String
    } deriving (Generic, Show)

data SearcherView = SearcherView
    { key      :: Maybe String
    , selected :: Int
    , entries  :: [EntryView]
    , input    :: String
    } deriving (Generic, Show)

instance ToJSON EntryView
instance ToJSON SearcherView

instance Convertible Match EntryView where
    convert m = EntryView
        {- name -} (m ^. Match.name . to convert)
        {- doc  -} (m ^. Match.doc . to convert)
        {- tpe  -} (m ^. Match.entryType . to show)

instance Convertible Searcher SearcherView where
    convert s = SearcherView
        {- key      -} (s ^. Searcher.mode . to nodeKey')
        {- selected -} (s ^. Searcher.selected)
        {- entries  -} (s ^. Searcher.mode . to entries')
        {- input    -} "test"

nodeKey' :: Searcher.Mode -> Maybe String
nodeKey' = \case
    Searcher.Node nl _ _   -> Just $ show nl
    Searcher.NodeName nl _ -> Just $ show nl
    _                      -> Nothing

entries' :: Searcher.Mode -> [EntryView]
entries' = \case
    Searcher.Command    m -> convert m
    Searcher.Node   _ _ m -> convert m
    Searcher.NodeName _ m -> convert m
    Searcher.PortName _ m -> convert m

foreign import javascript safe "atomCallback.getNodeEditorView().setSearcher($1)"
    setSearcher' :: JSVal -> IO ()

setSearcher :: MonadIO m => Maybe SearcherView -> m ()
setSearcher = liftIO . setSearcher' <=< toJSONVal
