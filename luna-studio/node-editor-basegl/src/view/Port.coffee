import * as basegl    from 'basegl'

import {ModelView, modelViewMixin}    from 'view/ModelView'

import * as shape     from 'shape/Port'
import {group}      from 'basegl/display/Symbol'
import {Composable, fieldMixin} from "basegl/object/Property"

inPortShape = basegl.symbol shape.inPortShape
inPortShape.bbox.xy = [shape.width,shape.length]


export class InPort extends ModelView
    constructor: (values, parent) ->
        super values, parent

    updateModel: ({key: @key = @key, nodePosition: @nodePosition = @nodePosition, angle: @angle = @angle}) =>
        unless @def?
            @def = inPortShape

    updateView: =>
        if @view?
            if @nodePosition?
                @group.position.xy = @nodePosition
                @view.position.xy = [-shape.width/2, @radius]
                @view.rotation.z = @angle

    registerEvents: =>


outPortShape = basegl.symbol shape.outPortShape
outPortShape.bbox.xy = [shape.width,shape.length]

export class OutPort extends ModelView
    constructor: (values, parent) ->
        super values, parent

    updateModel: ({key: @key, nodePosition: @nodePosition, angle: @angle}) =>
        unless @def?
            @def = outPortShape

    updateView: =>
        if @view?
            if @nodePosition?
                @group.position.xy = @nodePosition
                @view.position.xy = [-shape.width/2, @radius]
                @view.rotation.z = @angle

    registerEvents: =>
