import {Navigator}      from 'basegl/navigation/Navigator'

import {Breadcrumbs}    from 'view/Breadcrumbs'
import {Connection}     from 'view/Connection'
import {ExpressionNode} from 'view/ExpressionNode'
import {InputNode}      from 'view/InputNode'
import {OutputNode}     from 'view/OutputNode'
import {Port}           from 'view/Port'
import {Searcher}       from 'view/Searcher'


export class NodeEditor
    constructor: (@_scene) ->
        @nodes ?= {}
        @connections ?= {}

    withScene: (fun) => fun @_scene if @_scene?
    
    initialize: =>
        @withScene (scene) =>
            @controls = new Navigator scene

    node: (nodeKey) =>
        node = @nodes[nodeKey]
        if node? then node
        else if @inputNode?  and (@inputNode.key  == nodeKey) then @inputNode
        else if @outputNode? and (@outputNode.key == nodeKey) then @outputNode

    unsetNode: (node) =>
        if @nodes[node.key]?
            @nodes[node.key].detach()
            delete @nodes[node.key]

    setNode: (node) =>
        if @nodes[node.key]?
            @nodes[node.key].set node
        else
            nodeView = new ExpressionNode node, @
            @nodes[node.key] = nodeView
            nodeView.attach()

    setNodes: (nodes) =>
        for node in nodes
            @setNode node
        undefined

    setBreadcrumbs: (breadcrumbs) => @genericSetComponent 'breadcrumbs', Breadcrumbs, breadcrumbs
    setInputNode:   (inputNode)   => @genericSetComponent 'inputNode',   InputNode,   inputNode
    setOutputNode:  (outputNode)  => @genericSetComponent 'outputNode',  OutputNode,  outputNode
    setSearcher:    (searcher)    => @genericSetComponent 'searcher',    Searcher,    searcher

    unsetConnection: (connection) =>
        if @connections[connection.key]?
            @connections[connection.key].detach()
            delete @connections[connection.key]

    setConnection: (connection) =>
        if @connections[connection.key]?
            @connections[connection.key].set connection
        else
            connectionView = new Connection connection, @
            @connections[connection.key] = connectionView
            connectionView.attach()

    setConnections: (connections) =>
        for connection in connections
            @setConnection connection
        undefined

    genericSetComponent: (name, constructor, value) =>
        if value?
            if @[name]?
                @[name].set value
            else
                @[name] = new constructor value, @
                @[name].attach()
        else
            if @[name]?
                @[name].detach()
                @[name] = null
