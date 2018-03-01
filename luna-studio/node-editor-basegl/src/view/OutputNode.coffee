import {Component} from 'view/Component'
import * as shape  from 'shape/Port'
import * as basegl from 'basegl'
import {FlatPort}  from 'view/Port'


height = 100
windowLength = 1000

export class OutputNode extends Component
    updateModel: ({ key:      @key      = @key
                  , inPorts:  inPorts   = @inPorts
                  , position: @position = @position}) =>
        @setInPorts inPorts

        i = 0
        keys = Object.keys @inPorts
        portOffset = height / keys.length
        for key in keys
            inPort = @inPorts[key]
            inPort.set position: [windowLength - shape.length, i * portOffset]
            i++

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
            inPort.output = true
            portView = new FlatPort inPort, @
            @inPorts[inPort.key] = portView
            portView.attach()
