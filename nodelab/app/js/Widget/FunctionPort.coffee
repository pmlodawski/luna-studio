$$           = require('common')
config       = require('config')
colors       = require('colors')

BaseWidget   = require('Widget/BaseWidget')

class FunctionPort extends BaseWidget
  constructor: (widgetId, width, height, isInput) ->
    super widgetId, width, height

    @uniforms =
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      color:     { type: 'v4', value: new THREE.Vector4(50.0, 50, 10, 50.0) }
      isInput:    { type: 'i', value: if isInput then 1 else 0 }
      hovered:   { type: 'i', value: 0 }

    @uniforms[k] = v for k, v of $$.commonUniforms

    bgMesh = new THREE.PlaneBufferGeometry(1, 1)
    bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0))

    @bg = new THREE.Mesh bgMesh, new THREE.ShaderMaterial
        uniforms:       @uniforms
        vertexShader:   require('shaders/sdf.vert')()
        fragmentShader: require('shaders/function_port.frag')()
        transparent:    true
        blending:       THREE.NormalBlending
        side:           THREE.DoubleSide
        extensions:
          derivatives:    true

    @mesh.add @bg

    @bg.scale.set(width, height, 1.0);
    @container = new THREE.Group()
    @mesh.add @container

  setHovered: (hovered) -> @uniforms.hovered.value = if hovered then 1 else 0
  setColor: (color) -> @uniforms.color.value = colors[color]


module.exports = FunctionPort
