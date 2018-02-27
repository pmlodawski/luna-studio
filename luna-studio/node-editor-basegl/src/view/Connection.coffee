import * as basegl    from 'basegl'
import {group}        from 'basegl/display/Symbol'
import {Composable}   from "basegl/object/Property"

import * as shape       from 'shape/Connection'
import {Component} from 'view/Component'


connectionShape = basegl.symbol shape.connectionShape
connectionShape.bbox.y = shape.width

export class Connection extends Component
    constructor: (values, parent) ->
        super values, parent
        @srcNodeSubscirbed = false
        @dstNodeSubscribed = false

    updateModel: ({ key: @key = @key
                  , srcNode: @srcNode = @srcNode
                  , srcPort: @srcPort = @srcPort
                  , dstNode: @dstNode = @dstNode
                  , dstPort: @dstPort = @dstPort}) =>
        unless @def?
            @def = connectionShape

    updateView: =>
        if @view?
            @connectSources()
            srcNode = @parent.nodes[@srcNode]
            dstNode = @parent.nodes[@dstNode]
            x = dstNode.position[0] - srcNode.position[0]
            y = dstNode.position[1] - srcNode.position[1]
            length = Math.sqrt (x*x + y*y)
            @view.bbox.x = 2*length
            @group.position.xy = srcNode.position
            rotation = Math.atan2 y, x
            @view.rotation.z = rotation
            srcNode.outPorts[@srcPort].set angle: rotation - Math.PI/2
            dstNode.inPorts[@dstPort].set angle: rotation + Math.PI/2

    connectSources: =>
        unless @connected?
            @connected = true
            srcNode = @parent.nodes[@srcNode]
            srcNode.addEventListener 'position', @updateView
            dstNode = @parent.nodes[@dstNode]
            dstNode.addEventListener 'position', @updateView

    registerEvents: =>
