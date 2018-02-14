eventListeners = []

export subscribeEvents = (listener) =>
    eventListeners.push listener

export class ModelView

    pushEvent: (path, event) =>
        for listener in eventListeners
            listener path, event

    attach: (@scene) =>
        if @scene?
            @view = @scene.add @def
            @set @
            @registerEvents()
            @updateView()

    detach: (@scene) =>
        if @view?
            # @scene.remove @view
            @view = null

    reatach: =>
        if @scene?
            @detach @scene
            @attach @scene
