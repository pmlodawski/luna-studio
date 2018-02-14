eventListeners = []

export subscribeEvents = (listener) =>
    eventListeners.push listener

export class ModelView

    constructor: (values, @scene) ->
        @set values
        # @attach()

    pushEvent: (path, event) =>
        for listener in eventListeners
            listener path, event

    set: (values) =>
        @updateModel values
        @updateView()

    attach: =>
        if @scene? and @def?
            @view = @scene.add @def
            @registerEvents()
            @updateView()

    detach: =>
        if @view?
            # @scene.remove @view
            @view = null

    reatach: =>
        @detach()
        @attach()
