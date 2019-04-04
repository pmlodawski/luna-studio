Here you can find most important modules of node-editor application:

* [Action](Action) - contains actions concept, allowing to perform operations on gui [State](State). The most important here is [Action/Basic](Action/Basic) module, which contain higher level operations like [addConnection](Action/Basic/AddConnection.hs), [addNode](Action/Basic/AddNode.hs),  [collapseToFunction](Action/Basic/CollapseToFunction.hs) and so on.
* [Batch/Connector](Batch/Connector) - module responsible for performin operations on backend. For example you will find there [addConnection](Batch/Connector/Commands.hs) method, which will send request to backend, to perform that operation.
* [Event](Event) - this is a module responsible for both event gathering and processing. You can find also event loop which is started with application launch.
* [Handler](Handler) - contains set of handlers, which are delegated to handle events in the application
* [State](State) - global application state
