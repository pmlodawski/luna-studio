Most of this files, except **View.hs** are deprecated.

* [Atom.hs](Atom.hs) contains FFI calls which are forwarding events from and to atom application
* [Clipboard.hs](Clipboard.hs) contains a call to [js function](../../vendor/clipboard.js) allowing to copy data to clipboard 
* [Config.hs](Config.hs) is legacy config handling file
* [DownloadFile.hs](DownloadFile.hs) this is also legacy, we are not using this
* [Event.hs](Event.hs) legacy
* [FontSize.hs](FontSize.hs) legacy
* [UUID.hs](UUID.hs) is a legacy file for creating UUIDs in browser. Haskell library for that didn't compile on GHCJS
* [View.hs](View.hs) contains FFI calls allowing to forward events from and to basegl-ui application
* [Visualizers.hs](Visualizers.hs) - visualizers related FFI calls
