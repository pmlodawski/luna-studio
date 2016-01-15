$$ = require('common')
vs = require('shaders/select.vert')()
fs = require('shaders/select.frag')()
color = new (THREE.Vector4)(0.85, 0.55, 0.1, 0.2)

class SelectionBox
  constructor: ->
    geometry = new (THREE.PlaneBufferGeometry)(1, 1)
    geometry.applyMatrix (new (THREE.Matrix4)).makeTranslation(0.5, 0.5, 0.0)

    @uniforms =
      size:    {type: 'v2', value: new (THREE.Vector2)(0, 0)}
      color:   {type: 'v4', value: color}

    @uniforms[k] = v for k, v of $$.commonUniforms

    @mesh = new (THREE.Mesh)(geometry, new (THREE.ShaderMaterial)(
      uniforms: @uniforms
      vertexShader: vs
      fragmentShader: fs
      transparent: true
      blending: THREE.NormalBlending
      side: THREE.DoubleSide))

    @mesh.position.z = 1

  setPos: (x0, y0, x1, y1) ->
    w = Math.max(Math.abs(x1 - x0), 1.0)
    h = Math.max(Math.abs(y1 - y0), 1.0)
    x = Math.min(x1, x0)
    y = Math.min(y1, y0)

    @mesh.position.set x, y, 1.0
    @mesh.scale.set w, h, 1.0
    @uniforms.size.value.set w, h, 1.0

  show: -> @mesh.visible = true
  hide: -> @mesh.visible = false

module.exports = SelectionBox
