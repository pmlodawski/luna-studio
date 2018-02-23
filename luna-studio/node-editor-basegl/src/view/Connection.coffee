import * as basegl    from 'basegl'
import {group}        from 'basegl/display/Symbol'

import {ModelView}    from 'view/ModelView'
import * as shape     from 'shape/Connection'


connectionShape = basegl.symbol shape.connectionShape
connectionShape.bbox.y = shape.width

export class Connection extends ModelView
    constructor: (values, scene, @nodeEditor) ->
        super values, scene
        @srcNodeSubscirbed = false
        @dstNodeSubscribed = false

    updateModel: ({key: @key = @key, srcNode: @srcNode = @srcNode, srcPort: @srcPort = @srcPort, dstNode: @dstNode = @dstNode, dstPort: @dstPort = @dstPort}) =>
        unless @def?
            @def = connectionShape

    updateView: =>
        if @view?
            @connectSources()
            srcNode = @nodeEditor.nodes[@srcNode]
            dstNode = @nodeEditor.nodes[@dstNode]
            x = dstNode.position[0] - srcNode.position[0]
            y = dstNode.position[1] - srcNode.position[1]
            length = Math.sqrt (x*x + y*y)
            @view.bbox.x = 2*length
            @group.position.xy = srcNode.position
            @view.rotation.z = Math.atan2 y, x

    connectSources: =>
        unless @srcNodeSubscirbed
            @nodeEditor.nodes[@srcNode].subscribeProperty 'position', =>
                @srcNodeSubscirbed = false
                @updateView()
            @srcNodeSubscirbed = true
        unless @dstNodeSubscribed
            @nodeEditor.nodes[@dstNode].subscribeProperty 'position', =>
                @dstNodeSubscribed = false
                @updateView()
            @dstNodeSubscribed = true

    registerEvents: =>
