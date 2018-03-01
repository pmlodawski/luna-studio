import {Navigator}      from 'basegl/navigation/Navigator'

import {Breadcrumbs}    from 'view/Breadcrumbs'
import {Connection}     from 'view/Connection'
import {ExpressionNode} from 'view/ExpressionNode'
import {InputNode}      from 'view/InputNode'
import {OutputNode}     from 'view/OutputNode'
import {Port}           from 'view/Port'


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

    setInputNode: (inputNode) =>
        if inputNode?
            if @inputNode?
                @inputNode.set inputNode
            else
                @inputNode = new InputNode inputNode, @
                @inputNode.attach()
        else
            if @inputNode?
                @inputNode.detach()
                @inputNode = null

    setOutputNode: (outputNode) =>
        if outputNode?
            if @outputNode?
                @outputNode.set outputNode
            else
                @outputNode = new OutputNode outputNode, @
                @outputNode.attach()
        else
            if @outputNode?
                @outputNode.detach()
                @outputNode = null

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

    setBreadcrumbs: (breadcrumbs) =>
        if breadcrumbs?
            if @breadcrumbs?
                @breadcrumbs.set breadcrumbs
            else
                @breadcrumbs = new Breadcrumbs breadcrumbs, @
                @breadcrumbs.attach()
        else
            if @breadcrumbs?
                @breadcrumbs.detach()
                @breadcrumbs = null


# expressionNodes          :: ExpressionNodesMap
# inputNode                :: Maybe InputNode
# outputNode               :: Maybe OutputNode
# monads                   :: [MonadPath]
# connections              :: ConnectionsMap
# visualizersLibPath       :: FilePath
# nodeVisualizations       :: Map NodeLoc NodeVisualizations
# visualizationsBackup     :: VisualizationsBackupMap
# halfConnections          :: [HalfConnection]
# connectionPen            :: Maybe ConnectionPen
# selectionBox             :: Maybe SelectionBox
# searcher                 :: Maybe Searcher
# textControlEditedPortRef :: Maybe InPortRef
# graphStatus              :: GraphStatus
# layout                   :: Layout
# topZIndex                :: Int