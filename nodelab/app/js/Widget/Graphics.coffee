$$           = require('common')
vs           = require('shaders/graphics_sdf.vert')()
config       = require('config')

textAlign    = require('Text2D/textAlign')
Text2D       = require('Text2D/Text2D')

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

    @itemGroup = new THREE.Group()
    @mesh.add @itemGroup

    @labelGroup = new THREE.Group()
    @mesh.add @labelGroup

    @relayout()

  setItems: (items) ->
    @items = items
    newchildren = @items.map (item) =>
      uniforms =
           objectId:  @uniforms.objectId
           boxSize:   { type: 'v2', value: new THREE.Vector2(item._boxSize._x,   item._boxSize._y  )}
           boxOffset: { type: 'v2', value: new THREE.Vector2(item._boxOffset._x, item._boxOffset._y)}
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
    @itemGroup.remove(@itemGroup.children)
    newchildren.forEach (it) => @itemGroup.add it
    @relayout()

  setLabels: (labels) ->
    @labels = labels
    @labelGroup.remove @labelGroup.children
    @labels.forEach (label) =>

        align = textAlign.center

        cf = $$.commonUniforms.camFactor.value
        fontSize = (item._fontSize * cf).toFixed(2)
        label = new Text2D(label._text, { align: align, font: fs + 'px "Futura"', fillStyle: '#ffffff', antialias: true })
        label.rotation.x = Math.PI
        label.position.x = @width / 2.0
        label.scale.x = 1.0 / cf
        label.scale.y = 1.0 / cf

        @labelGroup.add label

  relayout: ->
    @mesh.scale.x = @width
    @mesh.scale.y = @height

  redrawTextures: ->
    @setLabels @labels

module.exports = Graphics;

