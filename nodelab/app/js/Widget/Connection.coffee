$$ = require('common')
colors = require('colors')
vs = require('shaders/sdf.vert')()
fs = require('shaders/connection.frag')()


class Connection
  constructor: (widgetId) ->
    @geometry = new (THREE.PlaneBufferGeometry)(1.0, 16.0)

    @uniforms =
      color:        {type: 'v4', value: colors[5]}
      visible:      {type: 'f',  value: 0}
      connecting:   {type: 'i',  value: if widgetId == 3 then 1 else 0}
      size:         {type: 'v2', value: new THREE.Vector2(1.0, 16.0)}
      highlight:    {type: 'i',  value: 0}
      arrow:        {type: 'i',  value: 1}
      objectId:     {type: 'v3', value: new (THREE.Vector3)(widgetId % 256 / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0)}

    @uniforms[k] = v for k, v of $$.commonUniforms

    @mesh = new (THREE.Mesh)(@geometry, new (THREE.ShaderMaterial)(
      uniforms: @uniforms
      vertexShader: vs
      fragmentShader: fs
      transparent: true
      blending: THREE.NormalBlending
      side: THREE.DoubleSide
      extensions:
        derivatives: true
      ))

    @mesh.position.z = -2

  setPos: (x0, y0, x1, y1) ->
    dist = 0
    x = x1 - x0
    y = y1 - y0
    r = Math.sqrt(x * x + y * y)
    x_r = x / r
    y_r = y / r
    @mesh.material.uniforms.size.value.x = r
    @mesh.scale.x = Math.max(r - (2 * dist), 0)
    scale = @mesh.scale.x / 2
    @mesh.rotation.z = Math.sign(y) * Math.acos(x_r)
    @mesh.position.x = dist * x_r + x0 + x_r * scale
    @mesh.position.y = dist * y_r + y0 + y_r * scale

  setVisible: (visible) -> @mesh.visible           = visible
  setColor:   (colorId) -> @uniforms.color.value   = colors[colorId]
  setHighlight: (val)   -> @uniforms.highlight.value    = val
  setArrow:   (arrow)   -> @uniforms.arrow.value   = if arrow then 1 else 0
  widgetMoved: -> null
module.exports = Connection

