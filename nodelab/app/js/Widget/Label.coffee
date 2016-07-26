$$           = require('common')
config       = require('config')

textAlign    = require('Text2D/textAlign')
Text2D       = require('Text2D/Text2D')

BaseWidget   = require('Widget/BaseWidget')

calculateTextWidth = (txt) -> layoutText(font: font, text: txt).width

class Label extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @alignment = 'Left'
    @text = ""

    @uniforms =
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
      color:     { type: 'v4', value: new THREE.Vector4(1.0, 0, 0, 0.0) }
      radius:    { type: 'v4', value: new THREE.Vector4(0.0, 0.0, 0.0, 0.0) }
    @uniforms[k] = v for k, v of $$.commonUniforms

    bgMesh = new THREE.PlaneBufferGeometry(1, 1)
    bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0))

    @bg = new THREE.Mesh bgMesh, new THREE.ShaderMaterial
        uniforms:       @uniforms
        vertexShader:   require('shaders/sdf.vert')()
        fragmentShader: require('shaders/generic_bg.frag')()
        transparent:    true
        blending:       THREE.NormalBlending
        side:           THREE.DoubleSide
        derivatives:    true

    @mesh.add @bg

    @bg.scale.set(width, height, 1.0);

  setAlignment: (align) ->
    @alignment = align
    @setLabel @text

  setLabel: (text) ->
    @text = text
    @mesh.remove @label if @label
    if @text and @text != ""
      align = switch @alignment
        when 'Left'   then textAlign.bottomLeft
        when 'Center' then textAlign.bottomCenter
        when 'Right'  then textAlign.bottomRight
        else throw 'Invalid text alignment'

      cf = $$.commonUniforms.camFactor.value

      @label = new Text2D(@text, { align: align, zoom: $$.commonUniforms.camFactor.value})
      @label.position.x = switch @alignment
        when 'Left'   then 0
        when 'Center' then @width / 2.0
        when 'Right'  then @width
        else throw 'Invalid text alignment'
      @label.position.y = @height / 2.0

      @mesh.add @label

      bgWidth = Math.max(@label.width / cf, 40.0)
      @bg.position.x = switch @alignment
        when 'Left'   then 0
        when 'Right'  then (@width - bgWidth)
        when 'Center' then (@width - bgWidth) / 2.0
        else throw 'Invalid text alignment'
      @bg.scale.x = bgWidth
    @bg.visible = (@text != "")

  relayout: ->
    super
    @setLabel @text
  redrawTextures: ->
    @label.setZoom $$.commonUniforms.camFactor.value if @label

module.exports = Label
