import * as basegl    from 'basegl'
import * as Animation from 'basegl/animation/Animation'
import * as Easing    from 'basegl/animation/Easing'
import * as Color     from 'basegl/display/Color'
import {world}        from 'basegl/display/World'
import {ModelView}    from 'view/ModelView'
import {circle, glslShape, union, grow, negate, rect, quadraticCurve, path} from 'basegl/display/Shape'


nodeRadius     = 30
gridElemOffset = 18
arrowOffset    = gridElemOffset + 2

export nodeSelectionBorderMaxSize = 40

white          = Color.rgb [1,1,1]
bg             = (Color.hsl [40,0.08,0.09]).toRGB()
selectionColor = bg.mix (Color.hsl [50, 1, 0.6]), 0.8
nodeBg         = bg.mix white, 0.04

export nodeShape = basegl.expr ->
    border       = 0
    bodyWidth    = 200
    bodyHeight   = 300
    slope        = 20
    r1    = nodeRadius + border

    headerShape   = circle(r1)
    header        = headerShape.move(nodeRadius,nodeRadius)

    node          = header.move(nodeSelectionBorderMaxSize,nodeSelectionBorderMaxSize)
    node          = node.fill nodeBg

    eye           = 'scaledEye.z'
    border        = node.grow(Math.pow(Math.clamp(eye*20.0, 0.0, 400.0),0.7)).grow(-1)

    sc            = selectionColor.copy()
    sc.a          = 'selected'
    border        = border.fill sc

    border + node

export expandedNodeShape = basegl.expr ->
    border       = 0
    bodyWidth    = 200
    bodyHeight   = 300
    slope        = 20
    headerOffset = arrowOffset
    r1    = nodeRadius + border
    r2    = nodeRadius + headerOffset + slope - border
    dy    = slope
    dx    = Math.sqrt ((r1+r2)*(r1+r2) - dy*dy)
    angle = Math.atan(dy/dx)

    maskPlane     = glslShape("-sdf_halfplane(p, vec2(1.0,0.0));").moveX(dx)
    maskRect      = rect(r1+r2, r2 * Math.cos(-angle)).alignedTL.rotate(-angle)
    mask          = (maskRect - maskPlane).inside
    headerShape   = (circle(r1) + mask) - circle(r2).move(dx,dy)
    headerFill    = rect(r1*2, nodeRadius + headerOffset + 10).alignedTL.moveX(-r1)
    header        = (headerShape + headerFill).move(nodeRadius,nodeRadius).moveY(headerOffset+bodyHeight)

    body          = rect(bodyWidth + 2*border, bodyHeight + 2*border, 0, nodeRadius).alignedBL
    node          = (header + body).move(nodeSelectionBorderMaxSize,nodeSelectionBorderMaxSize)
    node          = node.fill nodeBg

    eye           = 'scaledEye.z'
    border        = node.grow(Math.pow(Math.clamp(eye*20.0, 0.0, 400.0),0.7)).grow(-1)

    sc            = selectionColor.copy()
    sc.a          = 'selected'
    border        = border.fill sc

    border + node
