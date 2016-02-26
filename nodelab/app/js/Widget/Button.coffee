$$           = require('common')
vs           = require('shaders/sdf.vert')()
fs           = require('shaders/generic_bg.frag')()
config       = require('config')

createText   = require('bmfont').render
font         = require("font/default")
textMaterial = require('font/text_material').hud
layoutText   = require('bmfont').layout

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



module.exports = Button;
