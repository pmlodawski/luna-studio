BaseWidget   = require ('Widget/BaseWidget')
vs           = require('shaders/sdf.vert')()


class Group extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height
    @container = @mesh

    @bgUniforms =
      size:      { type: 'v2', value: new THREE.Vector2(width, height) }
      objectId:  { type: 'v3', value: new THREE.Vector3(0, 0, 0) }
      color:     { type: 'v3', value: new THREE.Vector3(1.0, 0, 0) }

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
  setBgVisible: (vis)   -> @bg.visible = vis

  relayout: ->
    @bg.scale.x = @width
    @bg.scale.y = @height
    @bgUniforms.size.value.set @width, @height

module.exports = Group;
