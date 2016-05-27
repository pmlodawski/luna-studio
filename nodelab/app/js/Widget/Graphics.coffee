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
     @mesh.remove(@mesh.children)
     @items.forEach (item) =>

       geom = new THREE.GraphicsBufferGeometry(item._boxes)
       item = new THREE.Mesh geom, new THREE.ShaderMaterial
                  uniforms: @uniforms
                  vertexShader:   vs
                  fragmentShader: item._shader
                  transparent:    true
                  blending:       THREE.NormalBlending
                  derivatives:    true
       @mesh.add item

     @relayout()

  relayout: ->
    @mesh.scale.x = @width
    @mesh.scale.y = @height

module.exports = Graphics;

