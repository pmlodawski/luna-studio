textAlign  = require('./textAlign')
CanvasText = require('./CanvasText')
$$         = require('common')
fs         = require('shaders/texFont.frag')
vs         = require('shaders/texFont.vert')

getTopParent = (w) ->
  p = w
  while p != undefined and p != null
    w = p
    p = w.parent
  w

class Text2D extends THREE.Object3D

  constructor: (text = '', options = {}) ->
    super()

    @font      = options.font      || 'Roboto, sans-serif';
    @fontSize  = options.fontSize  || 13;
    @fillStyle = options.fillStyle || '#ffffff';
    @zoom      = if (getTopParent this) == $$.sceneHUD then 1.0 else options.zoom || 1.0

    @canvas = new CanvasText()

    @align = options.align || textAlign.center
    @side  = options.side  || THREE.DoubleSide

    @antialias = options.antialias || false
    @setText text

  # delegate raycast method to mesh instance
  raycast: -> @mesh.raycast.apply(@mesh, arguments)

  width:  -> @canvas.textWidth
  height: -> @canvas.textHeight

  setText: (value) ->
    if @text != value
      @text = value
      @updateText()

  setFont: (value) ->
    if @font != value
      @font = value
      @updateText()

  setFontSize: (value) ->
    if @fontSize != value
      @fontSize = value
      @updateText()

  setFillStyle: (value) ->
    if @fillStyle != value
      @fillStyle = value
      @updateText()

  setZoom: (zoom) ->
    if (getTopParent this) == $$.sceneHUD
      zoom = 1
    if @zoom != zoom
      @zoom = zoom
      @updateText()

  getFontStyle: ->
    (@fontSize * @zoom).toFixed(2) + 'px ' + @font

  updateText: ->
    @cleanUp() # cleanup previous texture

    @canvas.drawText @text, {
        font: @getFontStyle(),
        fillStyle: @fillStyle
      }

    @texture = new THREE.Texture(@canvas.canvas)
    @texture.needsUpdate = true;
    @applyAntiAlias()

    if !@material
      @material = new THREE.ShaderMaterial
          uniforms:
            texture: { type: "t", value: @texture }
            objectMap: $$.commonUniforms.objectMap
          vertexShader:   vs()
          fragmentShader: fs()
          transparent:    true
          blending:       THREE.NormalBlending
          side:           @side
          derivatives:    true
          depthWrite: false
          depthTest: false
    else
      @material.uniforms.texture.value = @texture

    if !@mesh
      @mesh = new THREE.Mesh(new THREE.PlaneGeometry(@canvas.width(), @canvas.height()), @material);
      @geometry = @mesh.geometry
      @add @mesh


    @mesh.position.x = ( (@canvas.width()  / 2) -  (@canvas.textWidth  / 2)) + ((@canvas.textWidth / 2) * @align.x)
    @mesh.position.y = (- @canvas.height() / 2) + ((@canvas.textHeight / 2)  * @align.y)

    @rotation.x = Math.PI
    @scale.x = 1.0 / @zoom
    @scale.y = 1.0 / @zoom


    # manually update geometry vertices
    @geometry.vertices[0].x = @geometry.vertices[2].x = -@canvas.width()  / 2
    @geometry.vertices[1].x = @geometry.vertices[3].x =  @canvas.width()  / 2
    @geometry.vertices[0].y = @geometry.vertices[1].y =  @canvas.height() / 2
    @geometry.vertices[2].y = @geometry.vertices[3].y = -@canvas.height() / 2
    @geometry.verticesNeedUpdate = true
    @visible = @canvas.width() > 0

  cleanUp: -> @texture.dispose() if @texture

  applyAntiAlias: ->
    if @antialias == false
      @texture.magFilter = THREE.NearestFilter
      @texture.minFilter = THREE.LinearMipMapLinearFilter
    else
      @texture.magFilter = THREE.NearestFilter
      @texture.minFilter = THREE.NearestFilter

module.exports = Text2D
