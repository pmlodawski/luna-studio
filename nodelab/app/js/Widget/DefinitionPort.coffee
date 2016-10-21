$$           = require('common')
config       = require('config')

BaseWidget   = require('Widget/BaseWidget')

class DefinitionPort extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @uniforms =
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      color:     { type: 'v4', value: new THREE.Vector4(50.0, 50, 10, 50.0) }
    @uniforms[k] = v for k, v of $$.commonUniforms

    bgMesh = new THREE.PlaneBufferGeometry(1, 1)
    bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0))

    @bg = new THREE.Mesh bgMesh, new THREE.ShaderMaterial
        uniforms:       @uniforms
        vertexShader:   require('shaders/sdf.vert')()
        fragmentShader: require('shaders/label_bg.frag')()
        transparent:    true
        blending:       THREE.NormalBlending
        side:           THREE.DoubleSide
        extensions:
          derivatives:    true

    @mesh.add @bg

    @bg.scale.set(width, height, 1.0);


module.exports = DefinitionPort
