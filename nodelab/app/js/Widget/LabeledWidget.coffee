$$           = require('common')
vs           = require('shaders/sdf.vert')()
config       = require('config')

createText   = require('bmfont').render
font         = require("font/LatoBlack-sdf")
textMaterial = require('font/text_material').hud

class LabeledWidget
  constructor: (widgetId, width, height) ->
    @widgetId = widgetId
    @width    = width
    @height   = height

    @bgUniforms =
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      focus:     { type: 'i',  value: 0   }
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }

    @bgUniforms[k] = v for k, v of $$.commonUniforms

    @container = @mesh = new THREE.Group()

    @createBg()

  bgShader: require('shaders/generic_bg.frag')()

  createBg: ->
    bgMesh = new THREE.PlaneBufferGeometry(1, 1)
    bgMesh.applyMatrix new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0)

    @bg = new THREE.Mesh bgMesh, new THREE.ShaderMaterial
      uniforms:       @bgUniforms
      vertexShader:   vs
      fragmentShader: @bgShader
      transparent:    true
      blending:       THREE.NormalBlending
      side:           THREE.DoubleSide
      derivatives:    true

    @mesh.add @bg

  setFocus: (value) -> @bgUniforms.focus.value = value ? 1 : 0

  setSize: (width, height) ->
    @width  = width
    @height = height
    @relayout()

  setLabel: (text)  ->
    @mesh.remove @label if @label

    geometry = createText
      text:  text
      font:  font
      align: 'left'

    material = textMaterial()

    @label = new THREE.Mesh geometry, material
    @label.scale.multiplyScalar config.fontSize

    @relayout()

    @mesh.add @label

  relayout: ->
    @bg.scale.x = @width
    @bg.scale.y = @height

    if @label
      @label.position.x = @height / 2.0
      @label.position.y = @height / 2.0 + 5.0

module.exports = LabeledWidget
