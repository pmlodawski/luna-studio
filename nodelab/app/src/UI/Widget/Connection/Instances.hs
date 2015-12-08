module UI.Widget.Connection.Instances where

import Utils.PreludePlus
import Object.Widget.Connection (Connection, CurrentConnection)
import Object.Widget.CompositeWidget (CompositeWidget, createWidget, updateWidget)

instance CompositeWidget Connection where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

instance CompositeWidget CurrentConnection where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

