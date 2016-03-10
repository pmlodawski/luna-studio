BaseWidget   = require ('Widget/BaseWidget')
vs           = require('shaders/sdf.vert')()
DOMURL       = window.URL || window.webkitURL || window;

loader = new THREE.TextureLoader();

shouldRender = require('app').shouldRender

class Image extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @texture = new THREE.Texture()

    @createPlotMesh()

  createPlotMesh: ->
    plotMesh = new THREE.PlaneBufferGeometry(1, 1)
    plotMesh.applyMatrix new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0)


    @plotUniforms =
      texScale:  { type: 'v2', value: new THREE.Vector2(1.0, 1.0) }
      size:      { type: 'v2', value: new THREE.Vector2(@width, @height) }
      objectId:  { type: 'v3', value: new THREE.Vector3(0, 0, 0) }
      map:       { type: 't',  value: @texture }

    @plotUniforms[k] = v for k, v of $$.commonUniforms

    @plot = new THREE.Mesh plotMesh, new THREE.ShaderMaterial
      uniforms:       @plotUniforms
      vertexShader:   vs
      fragmentShader: require('shaders/plot.frag')()
      transparent:    true
      blending:       THREE.NormalBlending
      side:           THREE.DoubleSide
      derivatives:    true

    @plot.scale.x = @width
    @plot.scale.y = @height
    @plot.visible = false

    @mesh.add @plot


  setData: (url) ->
    @data = url
    @renderData()

  renderData: ->
    cf = $$.commonUniforms.camFactor.value

    texWidth  = Math.pow(2, Math.ceil(Math.log2(cf * @width)))
    texHeight = Math.pow(2, Math.ceil(Math.log2(cf * @height)))

    texWidth2  = Math.pow(2, Math.ceil(Math.log2(@width)))
    texHeight2 = Math.pow(2, Math.ceil(Math.log2(@height)))

    overWidth =  texWidth  / (cf * @width)
    overHeight = texHeight / (cf * @height)

    loader.load @data, (tex) =>
      @plotUniforms.map.value = tex
      @plotUniforms.map.value.needsUpdate = true
      @plot.visible = true
      shouldRender()

  relayout: ->
    @renderData()

module.exports = Image
