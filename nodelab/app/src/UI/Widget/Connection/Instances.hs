module UI.Widget.Connection.Instances where

import           Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)
import           Object.Widget.Connection      (Connection, CurrentConnection)
import           Utils.PreludePlus

instance CompositeWidget Connection where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

instance CompositeWidget CurrentConnection where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

