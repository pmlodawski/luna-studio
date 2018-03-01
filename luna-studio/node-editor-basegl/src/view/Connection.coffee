import * as basegl    from 'basegl'
import {group}        from 'basegl/display/Symbol'
import {Composable}   from "basegl/object/Property"

import * as shape       from 'shape/Connection'
import * as nodeShape   from 'shape/Node'
import * as portShape   from 'shape/Port'
import {Component} from 'view/Component'
import {SidebarNode} from 'view/SidebarNode'



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
            connectionShape = basegl.symbol shape.connectionShape
            connectionShape.bbox.y = shape.width
            @def = connectionShape

    updateView: =>
        @connectSources()
        srcNode = @parent.node @srcNode
        dstNode = @parent.node @dstNode
        if srcNode instanceof SidebarNode
            srcPos = srcNode.outPorts[@srcPort].position
            leftOffset = 0
        else
            srcPos = srcNode.position
            leftOffset = nodeShape.height/2 + 1/4 * portShape.length
        if dstNode instanceof SidebarNode
            dstPos = srcNode.inPorts[@dstPort].position
            rightOffset = 0
        else
            dstPos = dstNode.position
            rightOffset = nodeShape.height/2 + 1/2 * portShape.length
        x = dstPos[0] - srcPos[0]
        y = dstPos[1] - srcPos[1]
        length = Math.sqrt(x*x + y*y) - leftOffset - rightOffset
        @view.position.x = leftOffset
        @view.position.y = -shape.width/4
        @view.bbox.x = 2 * length
        @group.position.xy = srcPos.slice()
        rotation = Math.atan2 y, x
        @view.rotation.z = rotation
        unless srcNode instanceof SidebarNode
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
