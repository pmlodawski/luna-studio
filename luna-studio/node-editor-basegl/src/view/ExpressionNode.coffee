import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {world}        from 'basegl/display/World'
import {circle, glslShape, union, grow, negate, rect, quadraticCurve, path} from 'basegl/display/Shape'

import {ModelView}    from 'view/ModelView'
import {InPort}       from 'view/Port'

import * as shape     from 'shape/Node'

nodeWidth = 200
nodeHeight = 350

### Utils ###

makeDraggable = (a) ->
    a.addEventListener 'mousedown', (e) ->
        if e.button != 0 then return
        s = basegl.world.activeScene
        fmove = (e) ->
            a.position.x += e.movementX * s.camera.zoomFactor
            a.position.y -= e.movementY * s.camera.zoomFactor
        window.addEventListener 'mousemove', fmove
        window.addEventListener 'mouseup', () =>
          window.removeEventListener 'mousemove', fmove

applySelectAnimation = (symbol, rev=false) ->
    if symbol.selectionAnimation?
    then symbol.selectionAnimation.reverse()
    else
        anim = Animation.create
            easing      : Easing.quadInOut
            duration    : 0.1
            onUpdate    : (v) -> symbol.variables.selected = v
            onCompleted :     -> delete symbol.selectionAnimation
        if rev then anim.inverse()
        anim.start()
        symbol.selectionAnimation = anim
        anim

selectedComponent = null
makeSelectable = (a) ->
    a.addEventListener 'mousedown', (e) ->
        if e.button != 0 then return
        symbol = e.symbol
        if selectedComponent == symbol then return
        applySelectAnimation symbol
        if selectedComponent
            applySelectAnimation selectedComponent, true
            selectedComponent.variables.zIndex = 1
        selectedComponent = symbol
        selectedComponent.variables.zIndex = -10

expandedNodeShape = basegl.symbol shape.expandedNodeShape
expandedNodeShape.variables.selected = 0
expandedNodeShape.bbox.xy = [nodeWidth + 2*shape.nodeSelectionBorderMaxSize, nodeHeight + 2*shape.nodeSelectionBorderMaxSize]

nodeShape = basegl.symbol shape.nodeShape
nodeShape.variables.selected = 0
nodeShape.bbox.xy = [nodeWidth + 2*shape.nodeSelectionBorderMaxSize, nodeHeight + 2*shape.nodeSelectionBorderMaxSize]


export class ExpressionNode extends ModelView
    constructor: (values, scene) ->
        super values, scene

    updateModel: ({key: @key, name: @name, expression: @expression, inPorts: inPorts = {}, outPorts: @outPorts, position: @position, selected: @selected, expanded: expanded}) ->
        @setInPorts inPorts
        if @expanded != expanded
            if expanded
                @def = expandedNodeShape
            else
                @def = nodeShape
            @expanded = expanded
            if @view?
                @reatach()

    setInPorts: (inPorts) =>
        @inPorts ?= {}
        if inPorts.length?
            for inPort in inPorts
                @setInPort inPort
        else
            for inPortKey in Object.keys inPorts
                @setInPort inPorts[inPortKey]

    setInPort: (inPort) =>
        if @inPorts[inPort.key]?
            @inPorts[inPort.key].set inPort
        else
            portView = new InPort inPort, @scene
            @inPorts[inPort.key] = portView
            portView.attach()

    updateView: =>
        if @view?
            @view.position.xy = [@position[0], -@position[1]]
            @view.variables.selected = if @selected then 1 else 0

    registerEvents: =>
        makeDraggable @view
        makeSelectable @view
        window.view = @view
        window.push = @pushEvent
        @view.addEventListener 'click', (e) => @pushEvent ['node-editor', 'node'], e
