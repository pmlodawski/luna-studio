Here there are two most important modules of gui global state. First, [Global](Global.hs) contains all the information about `backend`, currently performad `actions` and so on. It also refers to [UI](UI.hs) state, which contains everything that should be visible on scene.

In [UI](UI.hs), you can see, that application state is duplicated. It contains state before and after rendering, and a flag that something was modified.

To perform modifications in ui, you can simply use `modify` method which allows easy state operation. After all operations are done, [`renderIfNeeded :: Monad m => (App -> App -> m ()) -> State -> m State`](UI.hs) is called by [event engine](../Event/Processor.hs). Given [`appView :: MonadIO m => App -> App -> m ()`](../../../../node-editor-view/src/NodeEditor/View/App.hs) It performs comparison between old and new states, and redraws only what is required.
