textAlign = require('./textAlign')
CanvasText = require('./CanvasText')

class Text2D extends THREE.Object3D

  constructor: (text = '', options = {}) ->
    super()

    @font = options.font || '30px Arial';
    @fillStyle = options.fillStyle || '#FFFFFF';

    @canvas = new CanvasText()

    @align = options.align || textAlign.center
    @side  = options.side  || THREE.DoubleSide

    @antialias = options.antialias || true
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

  setFillStyle: (value) ->
    if @fillStyle != value
      @fillStyle = value
      @updateText()

  setZoom: (zoom) ->
    @zoom = zoom
    @updateText()

  updateText: ->
    @cleanUp() # cleanup previous texture

    @canvas.drawText @text, {
        font: @font,
        fillStyle: @fillStyle
      }

    @texture = new THREE.Texture(@canvas.canvas)
    @texture.needsUpdate = true;
    @applyAntiAlias()

    if !@material
      @material = new THREE.MeshBasicMaterial({ map: @texture, side: @side });
      @material.transparent = true
    else
      @material.map = @texture

    if !@mesh
      @mesh = new THREE.Mesh(new THREE.PlaneGeometry(@canvas.width, @canvas.height), @material);
      @geometry = @mesh.geometry
      @add @mesh


    @mesh.position.x = ( (@canvas.width  / 2) -  (@canvas.textWidth  / 2)) + ((@canvas.textWidth / 2) * @align.x)
    @mesh.position.y = (- @canvas.height / 2) + ((@canvas.textHeight / 2)  * @align.y)

    # manually update geometry vertices
    @geometry.vertices[0].x = @geometry.vertices[2].x = -@canvas.width  / 2
    @geometry.vertices[1].x = @geometry.vertices[3].x =  @canvas.width  / 2
    @geometry.vertices[0].y = @geometry.vertices[1].y =  @canvas.height / 2
    @geometry.vertices[2].y = @geometry.vertices[3].y = -@canvas.height / 2
    @geometry.verticesNeedUpdate = true

  cleanUp: -> @texture.dispose() if @texture

  applyAntiAlias: ->
    if @antialias == false
      @texture.magFilter = THREE.NearestFilter
      @texture.minFilter = THREE.LinearMipMapLinearFilter

module.exports = Text2D
