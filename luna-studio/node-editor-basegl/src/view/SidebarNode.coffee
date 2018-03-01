import {Component}    from 'view/Component'
import * as shape       from 'shape/Port'
import * as basegl    from 'basegl'

inPortShape = basegl.symbol shape.inPortShape
inPortShape.bbox.xy = [shape.width,shape.length]

export class SidebarNode extends Component
    updateModel: ({ key:      @key      = @key
                  , inPorts:  @inPorts  = @inPorts
                  , outPorts: @outPorts = @outPorts
                  , position: @position = @position}) =>
        unless @def?
            @def = inPortShape

    updateView: =>
    registerEvents: =>

# export class SidebarNode extends Composible
#     cons: (args...) -> @mixin Component args... 

#     updateModel: ({ports: @ports = @ports, mode: @mode = @mode}) =>

#     updateView: =>


# export class Component extends Composible
#     cons: (args...) -> @mixin X args...


# a = new SidebarNode {foo : 7, xx:18}

# SidebarMode:
#   AddRemove | MoveConnect
#
# InputNode:
#   inputNodeLoc      :: NodeLoc
#   inputSidebarPorts :: [OutPortTree OutPort]
#   inputIsDef        :: Bool
#   inputMode         :: SidebarMode
#
# OutputNode:
#   outputNodeLoc      :: NodeLoc
#   outputSidebarPorts :: InPortTree InPort
#   outputMode         :: SidebarMode
