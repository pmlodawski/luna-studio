$$           = require('common')
vs           = require('shaders/graphics_sdf.vert')()
config       = require('config')
app          = require('app')

textAlign    = require('Text2D/textAlign')
Text2D       = require('Text2D/Text2D')

BaseWidget   = require 'Widget/BaseWidget'
Rendering    = require 'Rendering'
require('Widget/GraphicsBufferGeometry')

class Graphics extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @uniforms =
      enabled:   { type: 'i',  value: 1 }
      dpr:       { type: 'f',  value: 1 }
      objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
    @uniforms[k] = v for k, v of $$.commonUniforms

    @scene  = new THREE.Scene()
    @camera = new THREE.OrthographicCamera(0, width, 0, height, 0, 120)
    @camera.position.z = 100

    @bufferTexture = new THREE.WebGLRenderTarget( 1024, 1024, {minFilter: THREE.NearestFilter, magFilter: THREE.NearestFilter} )

    @itemGroup = new THREE.Group()
    @scene.add @itemGroup

    @labelGroup = new THREE.Group()
    @scene.add @labelGroup


    ####

    plotMesh = new THREE.PlaneBufferGeometry(1, 1)
    plotMesh.applyMatrix new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0)

    @plotUniforms =
      texScale:  { type: 'v2', value: new THREE.Vector2(1.0, 1.0) }
      size:      { type: 'v2', value: new THREE.Vector2(@width, @height) }
      objectId:  { type: 'v3', value: new THREE.Vector3(1, 0, 0) }
      map:       { type: 't',  value: @bufferTexture.texture }

    @plotUniforms[k] = v for k, v of $$.commonUniforms

    @plot = new THREE.Mesh plotMesh, new THREE.ShaderMaterial
      uniforms:       @plotUniforms
      vertexShader:   require('shaders/sdf.vert')()
      fragmentShader: require('shaders/offscreen.frag')()
      transparent:    true
      blending:       THREE.NormalBlending
      side:           THREE.DoubleSide

    @plot.scale.x = @width
    @plot.scale.y = @height

    @mesh.add @plot

    @relayout()
    @requestRedraw()

  setItems: (items) ->
    # return
    @items = items
    @scene.remove @itemGroup
    @itemGroup = new THREE.Group()
    @scene.add @itemGroup
    # @itemGroup.children.forEach (obj) => @itemGroup.remove(obj)
    newchildren = @items.map (item, ix) =>
      uniforms =
           objectId:  @uniforms.objectId
           boxSize:   { type: 'v2', value: new THREE.Vector2(item._boxSize._x,   item._boxSize._y  )}
           boxOffset: { type: 'v2', value: new THREE.Vector2(item._boxOffset._x, item._boxOffset._y)}
      uniforms[k] = v for k, v of $$.commonUniforms
      geom = new THREE.GraphicsBufferGeometry(item._boxes, item._boxSize, item._boxOffset)
      shader = switch item._shader
        when "s1", "s2", "s3" then require('shaders/' + item._shader + '.frag')()
        else item._shader
      item = new THREE.Mesh geom, new THREE.ShaderMaterial
                 uniforms:       uniforms
                 vertexShader:   vs
                 fragmentShader: shader
                 transparent:    true
                 blending:       THREE.NormalBlending
                 extensions:
                   derivatives:    true
      item

    newchildren.forEach (it) => @itemGroup.add it
    @relayout()
    @requestRedraw()

  setLabels: (labels) ->
    # return
    @labels = labels
    @scene.remove @labelGroup
    @labelGroup = new THREE.Group()
    @scene.add @labelGroup

    # @labelGroup.children.forEach (obj) => @labelGroup.remove(obj)
    @labels.forEach (label) =>
      align = switch label._textAlignment
        when 'Left'   then textAlign.left
        when 'Center' then textAlign.center
        when 'Right'  then textAlign.right
        else throw 'Invalid text alignment'
      cf = $$.commonUniforms.camFactor.value
      textLabel = new Text2D(label._text, { align: align, zoom: $$.commonUniforms.camFactor.value, fontSize: label._fontSize })
      textLabel.position.x = label._labelPosition._x * @width
      textLabel.position.y = label._labelPosition._y * @height

      @labelGroup.add textLabel
    @requestRedraw()

  relayout: ->
    @itemGroup.scale.x = @width
    @itemGroup.scale.y = @height

  requestRedraw: ->
    Rendering.redrawTextureCallbacks.push (renderer) => @redrawTextures renderer

  redrawTextures: (renderer) ->
    @labelGroup.children.forEach (item) => item.setZoom $$.commonUniforms.camFactor.value
    $$.commonUniforms.objectMap.value = 0;
    $$.commonUniforms.antialias.value = 1;
    renderer.setClearColor(config.backgroundColor, 0);
    cf = $$.commonUniforms.camFactor.value
    @bufferTexture.setSize(@width * cf, @height * cf)
    @camera.right = @width * cf
    @camera.bottom = @height * cf
    renderer.render(@scene, @camera, @bufferTexture, true)

module.exports = Graphics;
