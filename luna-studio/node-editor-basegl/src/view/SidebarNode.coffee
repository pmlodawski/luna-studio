import {ModelView}    from 'view/ModelView'

export class SidebarNode extends ModelView
    constructor: (values, scene) ->
        super values, scene

    updateModel: ({ports: @ports, mode: @mode}) =>

    updateView: =>

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
