
export textWidth = (textGroup) =>
    textMinX = undefined
    textMaxX = undefined
    textGroup.children.forEach (child) =>
        l = child.position.x
        r = child.position.x + child.bbox.x
        textMinX = l unless l > textMinX
        textMaxX = r unless r < textMaxX
    textMaxX - textMinX