In this module, you can find:

* [Event loop](Loop.hs) which is started on application launch. You can see, that it based on `Control.Concurrent.Chan` which allows thread safe scheduling of events
* [Event sources](Source.hs) which are importing events from batch (`webSocketHandler`), basegl-ui (`viewHandler`) and atom (`viewHandler`)
* [Event processor](Processor.hs) which distributes gathered events to [Handlers](../Handler)
* Events themselves:
  * [Event](Event.hs) - common event datatype, which also contain depreacted, react based `UIEvents`
  * [Atom](Atom.hs) - events generated in atom
  * [Batch](Batch.hs) - events generated from Batch
  * [Connection](Connection.hs) - events related to websochet connection state
  * [View](View.hs) - events generated in basegl-ui
  
