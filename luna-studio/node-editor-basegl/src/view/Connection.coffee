import * as basegl    from 'basegl'
import {group}        from 'basegl/display/Symbol'

import {ModelView}    from 'view/ModelView'
import * as shape     from 'shape/Connection'


connectionShape = basegl.symbol shape.connectionShape
connectionShape.bbox.xy = [100, shape.width]

export class Connection extends ModelView
    constructor: (values, scene) ->
        super values, scene

    updateModel: ({key: @key, srcNode: @srcNode, srcPort: @srcPort, dstNode: @dstNode, dstPort: @dstPort}) =>
        unless @def?
            @def = connectionShape

    updateView: =>

    registerEvents: =>
