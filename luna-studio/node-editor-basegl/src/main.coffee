import * as basegl from 'basegl'

import {Connection}      from 'view/Connection'
import {ExpressionNode}  from 'view/ExpressionNode'
import {NodeEditor}      from 'view/NodeEditor'
import {SidebarNode}     from 'view/SidebarNode'
import {subscribeEvents} from 'view/Component'

export install = (name, f) ->
    scene = basegl.scene {domElement: name}
    basegl.fontManager.register 'DejaVuSansMono', 'rsc/DejaVuSansMono.ttf'
    await basegl.fontManager.load 'DejaVuSansMono',
        glyphSize: 20
        spread: 32

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
            outPorts: [{key: 1}
                      ,{key: 2}]
            position: [100, 100]
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
            position: [100, 400]
            expanded: false
            selected: false
        new ExpressionNode
            key: 3
            name: "foo bar baz"
            inPorts: [{key: 1}
                     ,{key: 2}]
            outPorts: [{key: 1}]
            position: [400, 100]
            expanded: false
            selected: false
        new ExpressionNode
            key: 4
            name: "node1"
            inPorts: [{key: 1}]
            outPorts: [{key: 1}]
            position: [400, 400]
            expanded: false
            selected: false
        ]

    nodeEditor.setInputNode new SidebarNode
        key: 8
        outPorts: [ {key: 1}
                  , {key: 2}
                  , {key: 3}]
    nodeEditor.setConnections [
        new Connection
            key: 0
            srcNode: 8
            srcPort: 2
            dstNode: 1
            dstPort: 1
        new Connection
            key: 1
            srcNode: 1
            srcPort: 1
            dstNode: 2
            dstPort: 1
        new Connection
            key: 2
            srcNode: 2
            srcPort: 1
            dstNode: 3
            dstPort: 1
        new Connection
            key: 3
            srcNode: 3
            srcPort: 1
            dstNode: 4
            dstPort: 1
        ]
    subscribeEvents (path, event) =>
        console.warn {path: path, base: event}
    window.n = nodeEditor

if NODE_EDITOR_EXAMPLE? then runExample()