$$           = require('common')
vs           = require('shaders/graphics_sdf.vert')()
config       = require('config')
createText   = require('bmfont').render
font         = require('font/default')
textMaterial = require('font/text_material').hud
layoutText   = require('bmfont').layout

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
    @itemGroup.remove(@itemGroup.children)
    newchildren.forEach (it) => @itemGroup.add it
    @relayout()

  setLabels: (labels) ->
    @labelGroup.remove @labelGroup.children
    # @labels.forEach (label) =>
    #   layout =
    #     text: label._text
    #     font: font
    #     align: 'Center'
    #   #  mode: 'pre'
    #   width = layoutText(layout).width * config.fontSize * label._fontSize
    #   #layout.width = @width / (config.fontSize)
    #   #width = Math.min width, @width
    #   geometry = createText layout
    #   material = textMaterial()

    #   label = new THREE.Mesh(geometry, material)
    #   label.scale.multiplyScalar config.fontSize * label._fontSize
    #   label.position.y = 0
    #   label.position.x = 0
    #     # switch label._textAlign
    #     # when 'Left'   then  0
    #     # when 'Right'  then  @width - width
    #     # when 'Center' then (@width - width) / 2.0
    #     # else throw 'Invalid text alignment'
    #   @labelGroup.add label

  relayout: ->
    @mesh.scale.x = @width
    @mesh.scale.y = @height

module.exports = Graphics;

