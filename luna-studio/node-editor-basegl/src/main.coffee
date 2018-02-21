import * as basegl from 'basegl'

import {ExpressionNode}  from 'view/ExpressionNode'
import {NodeEditor}      from 'view/NodeEditor'
import {subscribeEvents} from 'view/ModelView'

scene = null
nodeEditor = new NodeEditor -> scene

export getNodeEditor = -> nodeEditor

export install = (name) ->
    scene = basegl.scene {domElement: name}
    getNodeEditor().initialize()

export onEvent = subscribeEvents

main = -> install 'basegl-root'

window.run = main

runExample = ->
    main()
    nodeEditor.setNodes [
        new ExpressionNode
            key: 1
            name: "foo"
            inPorts: [{key: 1}]
            position: [100, 200]
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
            position: [300, 100]
            expanded: false
            selected: false
        ]
    subscribeEvents (path, event) =>
        console.warn {path: path, base: event}
    window.n = getNodeEditor()

if NODE_EDITOR_EXAMPLE? then runExample()