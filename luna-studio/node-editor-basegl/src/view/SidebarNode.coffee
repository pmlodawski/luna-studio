import {Component} from 'view/Component'
import * as shape  from 'shape/Port'
import * as basegl from 'basegl'
import {FlatPort}  from 'view/Port'


height = 100

export class SidebarNode extends Component
    updateModel: ({ key:      @key      = @key
                  , inPorts:  @inPorts  = @inPorts
                  , outPorts: outPorts = @outPorts
                  , position: @position = @position}) =>
        @setOutPorts outPorts

        i = 0
        keys = Object.keys @outPorts
        portOffset = height / keys.length
        for key in keys
            outPort = @outPorts[key]
            outPort.set position: [shape.length, i * portOffset]
            i++

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
            portView = new FlatPort outPort, @
            @outPorts[outPort.key] = portView
            portView.attach()
