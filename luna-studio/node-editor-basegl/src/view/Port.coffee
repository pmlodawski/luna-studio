import * as basegl  from 'basegl'
import {group}      from 'basegl/display/Symbol'
import {Composable} from "basegl/object/Property"

import * as shape       from 'shape/Port'
import {Component} from 'view/Component'

inPortShape = basegl.symbol shape.inPortShape
inPortShape.bbox.xy = [shape.width,shape.length]


export class InPort extends Component
    updateModel: ({ key:          @key          = @key
                  , nodePosition: @nodePosition = @nodePosition
                  , angle:        @angle        = @angle}) =>
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

export class OutPort extends Component
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
