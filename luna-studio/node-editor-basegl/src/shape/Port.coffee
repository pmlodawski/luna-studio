import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {world}        from 'basegl/display/World'
import {ModelView}    from 'view/ModelView'
import {circle, glslShape, union, grow, negate, rect, quadraticCurve, path, pie} from 'basegl/display/Shape'
import {nodeSelectionBorderMaxSize} from 'shape/Node'

angle = Math.PI/4
export length    = 10
export width     = length * Math.tan angle
distanceFromCenter = nodeSelectionBorderMaxSize
arrowRadius    = length + distanceFromCenter

export inPortShape = basegl.expr ->
    c =  circle arrowRadius
    c = c.move width/2, -distanceFromCenter
    p = pie angle
    p = p.rotate Math.PI
    p = p.move width/2, 0
    port = c * p
