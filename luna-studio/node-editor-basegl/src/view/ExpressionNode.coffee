import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {world}        from 'basegl/display/World'
import {circle, glslShape, union, grow, negate, rect, quadraticCurve, path} from 'basegl/display/Shape'
import {Composable, fieldMixin} from "basegl/object/Property"

import {InPort, OutPort} from 'view/Port'
import {Component}       from 'view/Component'
import * as shape        from 'shape/Node'
import * as util         from 'shape/util'

### Utils ###

makeDraggable = (a) ->
    a.group.addEventListener 'mousedown', (e) ->
        if e.button != 0 then return
        s = basegl.world.activeScene
        fmove = (e) ->
            x = a.position[0] + e.movementX * s.camera.zoomFactor
            y = a.position[1] - e.movementY * s.camera.zoomFactor
            a.set position: [x, y]
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


export class ExpressionNode extends Component
    updateModel: ({ key:        @key        = @key
                  , name:       @name       = @name
                  , expression: @expression = @expression
                  , inPorts:     inPorts    = @inPorts
                  , outPorts:    outPorts   = @outPorts
                  , position:    position   = @position
                  , selected:   @selected   = @selected
                  , expanded:    expanded   = @expanded}) =>
        @emitProperty 'position', position
        @setInPorts inPorts
        @setOutPorts outPorts
        if @expanded != expanded
            if expanded
                @def = expandedNodeShape
            else
                txtDef = basegl.text
                    str: @name
                    fontFamily: 'DejaVuSansMono'
                @def = [{name: 'node', def: nodeShape}
                       ,{name: 'name', def: txtDef}]
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
            portView = new InPort inPort, @
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
            portView = new OutPort outPort, @
            @outPorts[outPort.key] = portView
            portView.attach()

    updateView: =>
        if @view?
            @view.node.position.xy = [-shape.width/2, -shape.height/2]
            textWidth = util.textWidth @view.name
            @view.name.position.xy = [-textWidth/2, shape.width/2]
            @group.position.xy = @position.slice()
            @view.node.variables.selected = if @selected then 1 else 0

            @drawInPorts()
            @drawOutPorts()

    drawInPorts: =>
        inPortNumber = 0
        inPortKeys = Object.keys @inPorts
        for inPortKey in inPortKeys
            inPort = @inPorts[inPortKey]
            unless inPort.angle?
                if inPortKeys.length == 1
                    angle = Math.PI/2
                else
                    angle = Math.PI * (0.25 + 0.5 * inPortNumber/(inPortKeys.length-1))
                inPort.set angle: angle
            inPortNumber++

    drawOutPorts: =>
        outPortNumber = 0
        outPortKeys = Object.keys @outPorts
        for outPortKey in outPortKeys
            outPort = @outPorts[outPortKey]
            unless outPort.angle?
                if outPortKeys.length == 1
                    angle = Math.PI*3/2
                else
                    angle = Math.PI * (1.25 + 0.5 * outPortNumber/(outPortKeys.length-1))
                outPort.set angle: angle
            outPortNumber++

    registerEvents: =>
        makeDraggable @, => @updateView()
        makeSelectable @view.node
        window.view = @view
        window.push = @pushEvent
        @group.addEventListener 'click', (e) => @pushEvent ['node-editor', 'node'], e
