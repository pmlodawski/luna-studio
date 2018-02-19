import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {world}        from 'basegl/display/World'
import {ModelView}    from 'view/ModelView'
import {circle, glslShape, union, grow, negate, rect, quadraticCurve, path, pie} from 'basegl/display/Shape'


angle = Math.PI/4

export length    = 100
export width     = length * Math.tan angle
distanceFromCenter = 200
arrowRadius    = length + distanceFromCenter



white          = Color.rgb [1,1,1]
bg             = (Color.hsl [40,0.08,0.09]).toRGB()
selectionColor = bg.mix (Color.hsl [50, 1, 0.6]), 0.8
nodeBg         = bg.mix white, 0.04

export inPortShape = basegl.expr ->
    r1    = length
    c =  circle arrowRadius
    c = c.move width/2, arrowRadius
    p = pie angle
    p = p.move width/2, length
    c * p