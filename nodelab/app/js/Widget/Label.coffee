$$ = require('common')
config = require('config')
createText = require('bmfont').render
font = require('font/default')
textMaterial = require('font/text_material').hud
layoutText = require('bmfont').layout

BaseWidget = require ('Widget/BaseWidget')

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
    if @text
      layout =
        text: text
        font: font
        align: @alignment
        mode: 'pre'
      width = layoutText(layout).width * config.fontSize
      layout.width = @width / (config.fontSize)
      width = Math.min width, @width
      geometry = createText layout
      material = textMaterial()

      @label = new THREE.Mesh(geometry, material)
      @label.scale.multiplyScalar config.fontSize
      @label.position.y = 5 + @height / 2.0
      @label.position.x = switch @alignment
        when 'Left'   then  0
        when 'Right'  then  @width - width
        when 'Center' then (@width - width) / 2.0
        else throw 'Invalid text alignment'
      @mesh.add @label

      @bg.position.x = Math.max(@label.position.x, -20.0)
      @bg.scale.x = Math.max(width, 40.0)

  relayout: ->
    super
    @setLabel @text
module.exports = Label
