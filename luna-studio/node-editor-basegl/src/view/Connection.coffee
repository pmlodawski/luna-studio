import * as basegl    from 'basegl'
import {group}        from 'basegl/display/Symbol'

import {ModelView}    from 'view/ModelView'
import * as shape     from 'shape/Connection'


connectionShape = basegl.symbol shape.connectionShape
connectionShape.bbox.y = shape.width

export class Connection extends ModelView
    constructor: (values, scene, @nodeEditor) ->
        super values, scene

    updateModel: ({key: @key, srcNode: @srcNode, srcPort: @srcPort, dstNode: @dstNode, dstPort: @dstPort}) =>
        unless @def?
            @def = connectionShape

    updateView: =>
        if @view?
            srcNode = @nodeEditor.nodes[@srcNode]
            dstNode = @nodeEditor.nodes[@dstNode]
            x = dstNode.group.position.x - srcNode.group.position.x
            y = dstNode.group.position.y - srcNode.group.position.y
            length = Math.sqrt (x*x + y*y)
            @view.bbox.x = 2*length
            @group.position.xy = srcNode.group.position.xy
            @view.rotation.z = Math.atan2 y, x

    registerEvents: =>
