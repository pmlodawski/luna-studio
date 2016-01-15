$$           = require('common')
vs           = require('shaders/sdf.vert')()
fs           = require('shaders/generic_bg.frag')()
config       = require('config')

createText   = require('bmfont').render
font         = require("font/default")
textMaterial = require('font/text_material').hud
layoutText   = require('bmfont').layout

BaseWidget   = require ('Widget/BaseWidget')

calculateTextWidth = (txt) -> layoutText({font: font, text: txt}).width

class Button extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @uniforms =
      enabled:   { type: 'i',  value: 1 }
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      focus:     { type: 'i',  value: 0 }
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

    @labelText = ""
    @relayout()


  setEnabled: (value) -> @uniforms.enabled.value = value ? 1 : 0

  setLabel: (text) ->
     @labelText = text
     @mesh.remove @label if @label

     geometry = createText
       text:  text
       font:  font
       align: 'center'

     @labelTextWidth = calculateTextWidth(text) * config.fontSize

     material = textMaterial()
     @label = new THREE.Mesh geometry, material
     @label.scale.multiplyScalar config.fontSize
     @mesh.add(@label)

     @relayout()

  setIcon: (icon) ->
     @iconShader = icon
     @mesh.remove @icon if @icon
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
      @label.position.x = @width / 2.0 - @labelTextWidth / 2.0
      @label.position.y = @height/ 2.0 + 5.0

    if @icon
      @icon.scale.set @height, @height, 1.0
      @icon.position.x = @width / 2.0 - @height / 2.0
      @icon.position.y = 0.0



module.exports = Button;
