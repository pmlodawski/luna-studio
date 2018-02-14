import * as basegl    from 'basegl'

import {ModelView}    from 'view/ModelView'

import * as shape     from 'shape/Port'

inPortShape = basegl.symbol shape.inPortShape

export class InPort extends ModelView
    constructor: (values, scene) ->
        super values, scene

    updateModel: ({key: @key}) =>
        unless @def?
            @def = inPortShape

    updateView: =>

    registerEvents: =>

# Mode: Normal | Invisible | Inactive | TypeNotMatched | Highlighted | Moved Position | NameEdit
#
# Port:
#   portId    :: i
#   name      :: Text
#   valueType :: TypeRep
#   state     :: PortState
#   mode      :: Mode
