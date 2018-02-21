import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {circle, pie}  from 'basegl/display/Shape'
import {nodeSelectionBorderMaxSize} from 'shape/Node'

angle = Math.PI/3
export length    = 10
export width     = length * Math.tan angle
distanceFromCenter = nodeSelectionBorderMaxSize
inArrowRadius    = length + distanceFromCenter
outArrowRadius    = distanceFromCenter

export inPortShape = basegl.expr ->
    r = inArrowRadius
    c =  circle r
    c = c.move width/2, -distanceFromCenter
    p = pie angle
    p = p.rotate Math.PI
    p = p.move width/2, 0
    port = c * p

export outPortShape = basegl.expr ->
    r = outArrowRadius
    c = circle r
    c = c.move width/2, 0
    p = pie angle
    h2 = length - r + r * Math.cos Math.asin ((2*length*Math.tan (angle/2))/r )
    p = p.move width/2, h2 + r
    port = p - c
    port.move 0, -r+length-h2
