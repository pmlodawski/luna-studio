$$           = require('common')
vs           = require('shaders/graphics_sdf.vert')()
config       = require('config')

BaseWidget   = require ('Widget/BaseWidget')
require('Widget/GraphicsBufferGeometry')

class Graphics extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @uniforms =
      enabled:   { type: 'i',  value: 1 }
      dpr:       { type: 'f',  value: 1 }
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
    @uniforms[k] = v for k, v of $$.commonUniforms

    @relayout()

  setItems: (items) ->
    @items = items
    newchildren = @items.map (item) =>
      uniforms =
           objectId:  @uniforms.objectId
           boxSize:   { type:'v2', value: new THREE.Vector2(item._boxSize._x, item._boxSize._y)}
           boxOffset: { type:'v2', value: new THREE.Vector2(item._boxOffset._x, item._boxOffset._y)}
      uniforms[k] = v for k, v of $$.commonUniforms
      geom = new THREE.GraphicsBufferGeometry(item._boxes, item._boxSize, item._boxOffset)
      item = new THREE.Mesh geom, new THREE.ShaderMaterial
                 uniforms:       uniforms
                 vertexShader:   vs
                 fragmentShader: item._shader
                 transparent:    true
                 blending:       THREE.NormalBlending
                 derivatives:    true
      item
    @mesh.remove(@mesh.children)
    newchildren.forEach (it) => @mesh.add it
    @relayout()

  relayout: ->
    @mesh.scale.x = @width
    @mesh.scale.y = @height

module.exports = Graphics;

