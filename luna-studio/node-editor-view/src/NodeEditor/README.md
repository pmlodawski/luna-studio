[Batch](Batch) and [Data](Data) modules contains datatypes useful for both event and gui state data

[Event/Shortcut](Event/Shortcut.hs) is a datatype designated to represent shortcut events from atom. It should be removed in basegl-ui 

[Event/UI](Event/UI.hs) is also legacy module, which repreents events from react. It should be removed in basegl-ui 

[React](React) is a module which contains datatypes describing gui state. The root state is [React/Model/App](React/Model/App.hs) which contains root of all ui datatypes hierarachy

[View](View) is a module responsible for comparing changes between old and new state in ui, and calling appropriate JS FFI methods, to push this state into basegl-ui app. Root here is [`appView :: MonadIO m => App -> App -> m ()`](View/App.hs) method, which is passed to [`renderIfNeeded`](../../../node-editor/src/NodeEditor/State/UI.hs). It calls subsequent methods, to compare state of breadcrumbs, node-editor and so on.
