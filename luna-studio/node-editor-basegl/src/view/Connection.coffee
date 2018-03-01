import * as basegl    from 'basegl'
import {group}        from 'basegl/display/Symbol'
import {Composable}   from "basegl/object/Property"

import * as shape       from 'shape/Connection'
import * as nodeShape   from 'shape/Node'
import * as portShape   from 'shape/Port'
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
            srcNode = @parent.node @srcNode
            dstNode = @parent.node @dstNode
            x = dstNode.position[0] - srcNode.position[0]
            y = dstNode.position[1] - srcNode.position[1]
            length = Math.sqrt(x*x + y*y) - nodeShape.height - 3/4* portShape.length
            @view.position.x = nodeShape.height/2 + portShape.length/2
            @view.position.y = -shape.width/4
            @view.bbox.x = 2 * length
            @group.position.xy = srcNode.position.slice()
            rotation = Math.atan2 y, x
            @view.rotation.z = rotation
            srcNode.outPorts[@srcPort]?.set angle: rotation - Math.PI/2
            dstNode.inPorts[@dstPort]?.set angle: rotation + Math.PI/2

    connectSources: =>
        unless @srcConnected?
            srcNode = @parent.node @srcNode
            if srcNode?
                srcNode.addEventListener 'position', => @updateView()
                @srcConnected = true
        unless @dstConnected
            dstNode = @parent.node @dstNode
            if dstNode?
                dstNode.addEventListener 'position', => @updateView()
                @dstConnected = true

    registerEvents: =>
