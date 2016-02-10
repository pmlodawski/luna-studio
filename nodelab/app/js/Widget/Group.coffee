BaseWidget   = require ('Widget/BaseWidget')
vs           = require('shaders/sdf.vert')()


class Group extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height
    @container = new THREE.Group()
    @mesh.add @container

    @bgVisible   = true
    @padding = {top: 0, right:0, bottom: 0, left: 0}
    @bgUniforms =
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      objectId:  { type: 'v3', value: new THREE.Vector3(0, 0, 0) }
      color:     { type: 'v3', value: new THREE.Vector3(1.0, 0, 0) }
      radius:    { type: 'v4', value: new THREE.Vector4(10.0, 10.0, 10.0, 10.0) }

    @bgUniforms[k] = v for k, v of $$.commonUniforms

    @createBg()

  bgShader: require('shaders/group.frag')()

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

    @bg.scale.x = @width
    @bg.scale.y = @height

    @mesh.add @bg

  setBgColor: (r, g, b) -> @bgUniforms.color.value.set(r, g, b)
  setBorderRadius: (a, b, c, d) -> @bgUniforms.radius.value.set(a, b, c, d)
  setPadding: (t, r, b, l) ->
    @padding = {top: t, right:r, bottom: b, left: l}
    @relayout()

  setBgVisible: (vis)   ->
    @bgVisible  = vis
    @bg.visible = vis && @width != 0 && @height != 0

  relayout: ->
    @bg.scale.x = @width
    @bg.scale.y = @height
    @container.position.x = @padding.left
    @container.position.y = @padding.top
    @setBgVisible(@bgVisible)
    @bgUniforms.size.value.set @width, @height

module.exports = Group;
