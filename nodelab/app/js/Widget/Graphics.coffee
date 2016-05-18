$$           = require('common')
vs           = require('shaders/sdf.vert')()
config       = require('config')

BaseWidget   = require ('Widget/BaseWidget')

class Graphics extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @uniforms =
      enabled:   { type: 'i',  value: 1 }
      size:      { type: 'v2', value: new THREE.Vector2(height, height) }
      focus:     { type: 'i',  value: 0 }
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }

    @relayout()

  setItems: (items) ->
     @items = items
     console.log(items)

     @items.forEach (item) =>

       geom = new THREE.Geometry()
       item._boxes.forEach (box) =>
         g = new THREE.PlaneGeometry(1, 1)
         g.translate(0.5, 0.5, 0)
         g.scale(box._boxSize._x, box._boxSize._y, 1.0)
         g.translate(box._boxPosition._x, box._boxPosition._y, 0)
         geom.merge(g)

       item = new THREE.Mesh geom, new THREE.ShaderMaterial
          uniforms: {}
          vertexShader:   vs
          fragmentShader: item._shader
          transparent:    true
          blending:       THREE.NormalBlending
          side:           THREE.DoubleSide
          derivatives:    true
       @mesh.add item

     @relayout()

  relayout: ->
    @mesh.scale.x = @width
    @mesh.scale.y = @height

module.exports = Graphics;
