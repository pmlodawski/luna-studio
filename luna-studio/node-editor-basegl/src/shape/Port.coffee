import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {world}        from 'basegl/display/World'
import {ModelView}    from 'view/ModelView'
import {circle, glslShape, union, grow, negate, rect, quadraticCurve, path, pie} from 'basegl/display/Shape'


arrowLength    = 100
distanceFromCenter = 200
arrowRadius    = arrowLength + distanceFromCenter



white          = Color.rgb [1,1,1]
bg             = (Color.hsl [40,0.08,0.09]).toRGB()
selectionColor = bg.mix (Color.hsl [50, 1, 0.6]), 0.8
nodeBg         = bg.mix white, 0.04

export inPortShape = basegl.expr ->
    r1    = arrowLength
    c =  circle arrowRadius
    angle = Math.PI/4
    x = arrowLength * Math.tan angle / 2
    c = c.move x, arrowRadius
    p = pie angle
    p = p.move x, arrowLength
    c * p