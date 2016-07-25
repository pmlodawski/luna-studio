$$           = require('common')
vs           = require('shaders/sdf.vert')()
config       = require('config')

textAlign    = require('Text2D/textAlign')
Text2D       = require('Text2D/Text2D')

BaseWidget   = require('Widget/BaseWidget')

class LabeledWidget extends BaseWidget
  constructor: (widgetId, width, height, transparent = false) ->
    super widgetId, width, height

    @bgUniforms =
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      focus:     { type: 'i',  value: 0   }
      color:     { type: 'v4', value: new THREE.Vector4(0.18, 0.18, 0.18, 1.0)   }
      rounded:   { type: 'i',  value: 1   }
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }

    @bgUniforms[k] = v for k, v of $$.commonUniforms

    @container = @mesh

    if transparent
      @bgShader = require('shaders/transparent.frag')()

    @createBg()
    @text = ""


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

  setLabel: (text) ->
    @text = text
    @mesh.remove @label if @label
    if @text and @text != ""
      align = textAlign.bottomLeft

      cf = $$.commonUniforms.camFactor.value
      fs = (13 * cf).toFixed(2)

      @label = new Text2D(@text, { align: align, font: fs + 'px "Futura"', fillStyle: '#ffffff', antialias: true })
      @label.rotation.x = Math.PI
      @label.position.x = 0
      @label.position.y = @height / 2.0

      @label.scale.x = 1.0 / cf
      @label.scale.y = 1.0 / cf

      @mesh.add @label

      @relayout()

  relayout: ->
    @bg.scale.x = @width
    @bg.scale.y = @height
    @bgUniforms.size.value.set @width,  @height

    if @label
      @label.position.x = @height / 2.0

  redrawTextures: ->
    @setLabel @text


module.exports = LabeledWidget
