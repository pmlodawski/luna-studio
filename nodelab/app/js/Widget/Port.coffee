$$         = require('common')
colors     = require('colors')
vs         = require('shaders/port.vert')()
fs         = require('shaders/port.frag')()

nodeRadius = 30.0
colorFar   = colors[2]
height     = 30.0
width      = 30.0
halfWidth  = width / 2.0
margin     = 0.0
dist       = nodeRadius + halfWidth + margin
nodeSize   = 30.0

class Port
  constructor: (widgetId) ->
    @uniforms =
      color:     {type: 'v4', value: new (THREE.Color)('red')}
      colorFar:  {type: 'v4', value: colorFar}
      highlight: {type: 'i',  value: 0}
      mouseDist: {type: 'f',  value: 0.0}
      nodeSize:  {type: 'f',  value: nodeSize}
      portSize:  {type: 'f',  value: height}
      objectId:  {type: 'v3', value: new (THREE.Vector3)(widgetId % 256 / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0)}

    @uniforms[k] = v for k, v of $$.commonUniforms

    @mesh = new (THREE.Mesh)(new (THREE.PlaneBufferGeometry)(width, height), new (THREE.ShaderMaterial)(
      uniforms: @uniforms
      vertexShader: vs
      fragmentShader: fs
      transparent: true
      blending: THREE.NormalBlending
      side: THREE.DoubleSide))

    @setAngle 0

  setAngle: (angle) ->
    @angle = angle
    @mesh.position.x = Math.cos(angle) * dist
    @mesh.position.y = Math.sin(angle) * dist
    @mesh.rotation.z = angle

  setColor:        (color)     -> @uniforms.color.value     = colors[color]
  setHighlight:    (val)       -> @uniforms.highlight.value = if val then 1 else 0
  updateMouseDist: (mouseDist) -> @uniforms.mouseDist.value = mouseDist

module.exports = Port

