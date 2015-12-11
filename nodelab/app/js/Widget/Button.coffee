$$           = require('common')
vs           = require('shaders/sdf.vert')()
fs           = require('shaders/generic_bg.frag')()
config       = require('config')

createText   = require('bmfont').render
font         = require("font/LatoBlack-sdf")
textMaterial = require('font/text_material').hud
layoutText   = require('bmfont').layout

calculateTextWidth = (txt) -> layoutText({font: font, text: txt}).width

class Button
  constructor: (widgetId, width, height) ->
    @widgetId = widgetId

    @uniforms =
      enabled:   { type: 'i',  value: 1 }
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      focus:     { type: 'i',  value: 0 }
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }

    @uniforms[k] = v for k, v of $$.commonUniforms

    this.mesh = new THREE.Group()

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

    @bg.scale.x    = width
    @bg.scale.y    = height
    @mesh.add @bg

   setEnabled: (value) -> @uniforms.enabled.value = value ? 1 : 0

   setLabel: (text) ->
      @mesh.remove @label if @label

      geometry = createText
        text:  text
        font:  font
        align: 'center'

      textWidth = calculateTextWidth(text) * config.fontSize

      material = textMaterial()
      @label = new THREE.Mesh geometry, material
      @label.scale.multiplyScalar config.fontSize
      @label.position.x = this.uniforms.size.value.x / 2.0 - textWidth / 2.0
      @label.position.y = this.uniforms.size.value.y / 2.0 + 5.0

      @mesh.add(@label)

module.exports = Button;
