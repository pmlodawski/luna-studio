import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {circle, pie, rect}  from 'basegl/display/Shape'
import {nodeSelectionBorderMaxSize} from 'shape/Node'

export width     = 5

export connectionShape = basegl.expr ->
    r = rect 'bbox.x', width

