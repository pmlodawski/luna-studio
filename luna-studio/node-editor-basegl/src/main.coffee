import * as basegl from 'basegl'

import {Connection}  from 'view/Connection'
import {ExpressionNode}  from 'view/ExpressionNode'
import {NodeEditor}      from 'view/NodeEditor'
import {subscribeEvents} from 'view/Component'

export install = (name, f) ->
    scene = basegl.scene {domElement: name}
    nodeEditor = new NodeEditor scene
    nodeEditor.initialize()
    f nodeEditor

export onEvent = subscribeEvents

main = (f) -> install 'basegl-root', f

window.run = main

runExample = -> main (nodeEditor) ->
    nodeEditor.setNodes [
        new ExpressionNode
            key: 1
            name: "foo"
            inPorts: [{key: 1}]
            outPorts: [{key: 1}]
            position: [100, 300]
            expanded: false
            selected: false
        new ExpressionNode
            key: 2
            name: "bar"
            inPorts: [{key: 1}
                     ,{key: 2}
                     ,{key: 3}
                     ,{key: 4}
                     ]
            outPorts: [{key: 1}]
            position: [500, 100]
            expanded: false
            selected: false
        new ExpressionNode
            key: 3
            name: "baz"
            inPorts: [{key: 1}]
            outPorts: [{key: 1}]
            position: [500, 400]
            expanded: false
            selected: false
        ]
    nodeEditor.setConnections [
        new Connection
            key: 1
            srcNode: 1
            srcPort: 1
            dstNode: 2
            dstPort: 4
        ]
    subscribeEvents (path, event) =>
        console.warn {path: path, base: event}
    window.n = nodeEditor

if NODE_EDITOR_EXAMPLE? then runExample()