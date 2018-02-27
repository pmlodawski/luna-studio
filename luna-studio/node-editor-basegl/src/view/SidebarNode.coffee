import {Component}    from 'view/Component'

export class SidebarNode extends Component
    updateModel: ({ key:      @key      = @key
                  , outPorts: @outPorts = @outPorts
                  , position: @position = @position}) =>

    updateView: =>

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
