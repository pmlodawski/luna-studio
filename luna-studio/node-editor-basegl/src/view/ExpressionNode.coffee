import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {world}        from 'basegl/display/World'
import {circle, glslShape, union, grow, negate, rect, quadraticCurve, path} from 'basegl/display/Shape'

import {ModelView}    from 'view/ModelView'
import {InPort, OutPort} from 'view/Port'

import * as shape     from 'shape/Node'

### Utils ###

makeDraggable = (a) ->
    a.view.addEventListener 'mousedown', (e) ->
        if e.button != 0 then return
        s = basegl.world.activeScene
        fmove = (e) ->
            a.position[0] += e.movementX * s.camera.zoomFactor
            a.position[1] -= e.movementY * s.camera.zoomFactor
            a?.updateView()
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
expandedNodeShape.bbox.xy = [shape.width + 2*shape.nodeSelectionBorderMaxSize, shape.height + 2*shape.nodeSelectionBorderMaxSize]

nodeShape = basegl.symbol shape.nodeShape
nodeShape.variables.selected = 0
nodeShape.bbox.xy = [shape.width, shape.height]


export class ExpressionNode extends ModelView
    constructor: (values, scene) ->
        super values, scene

    updateModel: ({key: @key, name: @name, expression: @expression, inPorts: inPorts = {}, outPorts: outPorts = {}, position: @position, selected: @selected, expanded: expanded}) ->
        @setInPorts inPorts
        @setOutPorts outPorts
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


    setOutPorts: (outPorts) =>
        @outPorts ?= {}
        if outPorts.length?
            for outPort in outPorts
                @setOutPort outPort
        else
            for outPortKey in Object.keys outPorts
                @setOutPort outPorts[outPortKey]

    setOutPort: (outPort) =>
        if @outPorts[outPort.key]?
            @outPorts[outPort.key].set outPort
        else
            portView = new OutPort outPort, @scene
            @outPorts[outPort.key] = portView
            portView.attach()

    updateView: =>
        if @view?
            @view.position.xy = [-shape.width/2, -shape.height/2]
            @group.position.xy = @position
            @view.variables.selected = if @selected then 1 else 0

            @drawInPorts()
            @drawOutPorts()

    drawInPorts: =>
        inPortNumber = 0
        inPortKeys = Object.keys @inPorts
        for inPortKey in inPortKeys
            inPort = @inPorts[inPortKey]
            inPort.nodePosition = [@group.position.x, @group.position.y]
            inPort.radius = shape.height/2
            if inPortKeys.length == 1
                inPort.angle = Math.PI/2
            else
                inPort.angle = Math.PI * (0.25 + 0.5 * inPortNumber/(inPortKeys.length-1))
            inPort.redraw()
            inPortNumber++

    drawOutPorts: =>
        outPortNumber = 0
        outPortKeys = Object.keys @outPorts
        for outPortKey in outPortKeys
            outPort = @outPorts[outPortKey]
            outPort.nodePosition = [@group.position.x, @group.position.y]
            outPort.radius = shape.height/2
            if outPortKeys.length == 1
                outPort.angle = Math.PI*3/2
            else
                outPort.angle = Math.PI * (1.25 + 0.5 * outPortNumber/(outPortKeys.length-1))
            outPort.redraw()
            outPortNumber++

    registerEvents: =>
        makeDraggable @, => @updateView()
        makeSelectable @view
        window.view = @view
        window.push = @pushEvent
        @view.addEventListener 'click', (e) => @pushEvent ['node-editor', 'node'], e
