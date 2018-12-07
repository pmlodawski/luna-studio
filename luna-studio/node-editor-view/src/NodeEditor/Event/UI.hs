{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.UI where

import           Common.Prelude

import           Common.Analytics                     (IsTrackedEvent (..))
import           Common.Data.Event                    (EventName (eventName), consName)
import qualified NodeEditor.React.Event.Searcher      as Searcher


data UIEvent
    = SearcherEvent      Searcher.Event
    deriving (Generic, NFData, Show, Typeable)

makePrisms ''UIEvent

instance EventName UIEvent where
    eventName event = consName event <> "." <> case event of
        SearcherEvent      ev -> eventName ev

instance IsTrackedEvent UIEvent where
    isTracked _ = False
