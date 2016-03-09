$$           = require('common')
vs           = require('shaders/sdf.vert')()
config       = require('config')

BaseWidget   = require ('Widget/BaseWidget')

class Icon extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @iconUniforms =
      enabled:   { type: 'i',  value: 1 }
      size:      { type: 'v2', value: new THREE.Vector2(height, height) }
      focus:     { type: 'i',  value: 0 }
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }

    @iconUniforms[k] = v for k, v of $$.commonUniforms

    @relayout()

  setIcon: (icon) ->
     @iconShader = icon
     @mesh.remove @icon if @icon
     if icon
       bgMesh = new THREE.PlaneBufferGeometry(1, 1)
       bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0))

       @icon = new THREE.Mesh bgMesh, new THREE.ShaderMaterial
          uniforms:       @iconUniforms
          vertexShader:   vs
          fragmentShader: require("shaders/icons/" + icon)()
          transparent:    true
          blending:       THREE.NormalBlending
          side:           THREE.DoubleSide
          derivatives:    true

       @mesh.add @icon

     @relayout()

  relayout: ->
    @iconUniforms.size.value.set @height, @height
    if @icon
      @icon.scale.set @height, @height, 1.0
      @icon.position.x = @width / 2.0 - @height / 2.0
      @icon.position.y = 0.0

module.exports = Icon;
