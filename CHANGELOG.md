# Flowbox Data changelog

The website http://flowbox.io/data/preview redirects always to the newest version!

## v 0.1 | 03.06.2015
* JavaScript preview of nodes (dragging)
* Proof of the concept for Haskell <-> JS bindings

## v 0.2 | [26.06.2015](http://flowbox.io/data/preview/2015-06-27/)

* New Haskell based reactive framework
  - It allows for future faster GUI development and proper event handling

* Completely new node editor
  - fully Haskell and WebGL based
  - displaying nodes with new shaders
  - selecting nodes
  - multi selection (holding alt)
  - priority selection - remembering selection history and last selected node
  - moving nodes
  - pan / zoom (left and middle mouse button)

## v 0.3 | [13.07.2015](http://flowbox.io/data/preview/2015-07-13/)

* Node editor development:
 - Create new nodes with new Node Searcher (Tab key)
 - Select multiple nodes by dragging
 - Move multiple nodes
 - Remove nodes
 - Display text (node labels) in WebGL
* Under the hood
 - Major rework of GUI framework
 - Feature switches in GUI aiming at Continous Delivery
