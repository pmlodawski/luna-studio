$$ = require('common')
vs = require('shaders/sdf.vert')()
fs = require('shaders/radio.frag')()
config = require('config')
createText = require('bmfont').render
font = require('font/LatoBlack-sdf')
textMaterial = require('font/text_material').hud
BaseWidget   = require ('Widget/BaseWidget')

class RadioButton extends BaseWidget
  constructor:  (widgetId, width, height) ->
    super widgetId, width, height

    @iconUniforms =
      size:     {type: 'v2', value: new (THREE.Vector2)(height, height)}
      value:    {type: 'i',  value: 0}

    @uniforms =
      size:     {type: 'v2', value: new (THREE.Vector2)(width, height)}
      focus:    {type: 'i',  value: 0}
      objectId: {type: 'v3', value: new (THREE.Vector3)(widgetId % 256 / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0)}

    for k, v of $$.commonUniforms
      @uniforms[k]     = v
      @iconUniforms[k] = v

    @createBg()
    @createIcon()
    @relayout()


  createBg: ->
    bgMesh = new (THREE.PlaneBufferGeometry)(1, 1)
    bgMesh.applyMatrix (new (THREE.Matrix4)).makeTranslation(0.5, 0.5, 0.0)

    @bg = new (THREE.Mesh)(bgMesh, new (THREE.ShaderMaterial)(
      uniforms: @uniforms
      vertexShader: vs
      fragmentShader: require('shaders/transparent.frag')()
      transparent: true
      blending: THREE.NormalBlending
      side: THREE.DoubleSide
      derivatives: true))

    @mesh.add @bg

  createIcon: ->
    bgMesh = new (THREE.PlaneBufferGeometry)(1, 1)
    bgMesh.applyMatrix (new (THREE.Matrix4)).makeTranslation(0.5, 0.5, 0.0)

    @icon = new (THREE.Mesh)(bgMesh, new (THREE.ShaderMaterial)(
      uniforms: @iconUniforms
      vertexShader: vs
      fragmentShader: fs
      transparent: true
      blending: THREE.NormalBlending
      side: THREE.DoubleSide
      derivatives: true))

    @mesh.add @icon

  relayout: ->
    @bg.scale.set @width, @height, 1.0
    @uniforms.size.value.set @width, @height

    @icon.scale.set @height, @height, 1.0
    @iconUniforms.size.value.set @height, @height

    if @label
      @label.position.x = 12.0 + @uniforms.size.value.y / 2.0
      @label.position.y =  5.0 + @uniforms.size.value.y / 2.0

  setValue: (value) -> @iconUniforms.value.value = if value then 1 else 0

  setFocus: (value) -> @iconUniforms.focus.value = if value then 1 else 0

  setLabel: (text) ->
    @mesh.remove @label if @label

    geometry = createText(
      text: text
      font: font
      align: 'left')
    material = textMaterial()

    @label = new (THREE.Mesh)(geometry, material)
    @label.scale.multiplyScalar config.fontSize
    @mesh.add @label

    @relayout()

module.exports = RadioButton
