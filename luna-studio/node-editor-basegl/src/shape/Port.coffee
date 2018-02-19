import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {world}        from 'basegl/display/World'
import {ModelView}    from 'view/ModelView'
import {circle, glslShape, union, grow, negate, rect, quadraticCurve, path} from 'basegl/display/Shape'


nodeRadius     = 10
gridElemOffset = 18
arrowOffset    = gridElemOffset + 2

export nodeSelectionBorderMaxSize = 40

white          = Color.rgb [1,1,1]
bg             = (Color.hsl [40,0.08,0.09]).toRGB()
selectionColor = bg.mix (Color.hsl [50, 1, 0.6]), 0.8
nodeBg         = bg.mix white, 0.04

export inPortShape = basegl.expr ->
    border       = 0
    bodyWidth    = 200
    bodyHeight   = 300
    slope        = 20
    r1    = nodeRadius + border

    headerShape   = circle r1
    # header        = headerShape.move(nodeRadius,nodeRadius)

    # node          = header.move(nodeSelectionBorderMaxSize,nodeSelectionBorderMaxSize)
    # node          = node.fill nodeBg

    # eye           = 'scaledEye.z'
    # border        = node.grow(Math.pow(Math.clamp(eye*20.0, 0.0, 400.0),0.7)).grow(-1)

    # sc            = selectionColor.copy()
    # sc.a          = 'selected'
    # border        = border.fill sc

    # border + node
