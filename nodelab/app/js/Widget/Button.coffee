$$           = require('common')
vs           = require('shaders/sdf.vert')()
fs           = require('shaders/generic_bg.frag')()
config       = require('config')

textAlign    = require('Text2D/textAlign')
Text2D       = require('Text2D/Text2D')

BaseWidget   = require ('Widget/BaseWidget')
Label        = require ('Widget/Label')



calculateTextWidth = (txt) -> layoutText({font: font, text: txt}).width

class Button extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @uniforms =
      enabled:   { type: 'i',  value: 1 }
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      focus:     { type: 'i',  value: 0 }
      rounded:   { type: 'i',  value: 1 }
      color:     { type: 'v4', value: new THREE.Vector4(0.18, 0.18, 0.18, 1.0)}
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }

    @uniforms[k] = v for k, v of $$.commonUniforms

    @iconUniforms =
      enabled:   { type: 'i',  value: 1 }
      size:      { type: 'v2', value: new THREE.Vector2(height, height) }
      focus:     { type: 'i',  value: 0 }
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }

    @iconUniforms[k] = v for k, v of $$.commonUniforms

    bgMesh = new THREE.PlaneBufferGeometry(1, 1)
    bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0))

    @bg = new THREE.Mesh bgMesh, new THREE.ShaderMaterial
        uniforms:       this.uniforms
        vertexShader:   vs
        fragmentShader: fs
        transparent:    true
        blending:       THREE.NormalBlending
        side:           THREE.DoubleSide
        derivatives:    true

    @mesh.add @bg

    @text = ""
    @alignment = 'Center'
    @relayout()

  setBgColor: (r, g, b, a) -> @uniforms.color.value.set(r, g, b, a)
  setEnabled: (value)      -> @uniforms.enabled.value = value ? 1 : 0
  setRounded: (value)      -> @uniforms.rounded.value = value ? 1 : 0

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
      fontSize = (13 * cf).toFixed(2)

      @label = new Text2D(@text, { align: align, font: fontSize + 'px "Futura"', fillStyle: '#ffffff', antialias: true })
      @label.rotation.x = Math.PI
      @label.position.x = switch @alignment
        when 'Left'   then 0
        when 'Center' then @width / 2.0
        when 'Right'  then @width
        else throw 'Invalid text alignment'
      @label.position.y = @height / 2.0

      @label.scale.x = 1.0 / cf
      @label.scale.y = 1.0 / cf

      @mesh.add @label

  redrawTextures: ->
    @setLabel @text

  relayout: ->
    @bg.scale.set @width, @height, 1.0
    @uniforms.size.value.set @width, @height

    @iconUniforms.size.value.set @height, @height
    if @label
      @setLabel @text

    if @icon
      @icon.scale.set @height, @height, 1.0
      @icon.position.x = @width / 2.0 - @height / 2.0
      @icon.position.y = 0.0

  setIcon: (icon) ->
    @iconShader = icon
    @mesh.remove @icon if @icon

    if icon
      bgMesh = new THREE.PlaneBufferGeometry(1, 1)
      bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0))
      @icon = new THREE.Mesh bgMesh, new THREE.ShaderMaterial
          uniforms:       this.uniforms
          vertexShader:   vs
          fragmentShader: require(icon)()
          transparent:    true
          blending:       THREE.NormalBlending
          side:           THREE.DoubleSide
          derivatives:    true
      @mesh.add @icon
    @relayout()

module.exports = Button;
