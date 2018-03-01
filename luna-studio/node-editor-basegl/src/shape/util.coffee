
export textWidth = (textGroup) =>
    textMinX = undefined
    textMaxX = undefined
    textGroup.children.forEach (child) =>
        l = child.position.x
        r = child.position.x + child.bbox.x
        textMinX = l unless l > textMinX
        textMaxX = r unless r < textMaxX
    textMaxX - textMinX

# addCaptureListener      'mousemove', (e) =>
#     @screenMouse.x = e.clientX
#     @screenMouse.y = e.clientY
#     campos = @camera.position
#     @mouse.x = (@screenMouse.x-@width/2 ) * campos.z + campos.x
#     @mouse.y = (@screenMouse.y-@height/2) * campos.z - campos.y
#     @_mouseBaseEvent = e
