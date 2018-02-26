import {group}      from 'basegl/display/Symbol'

eventListeners = []

export subscribeEvents = (listener) =>
    eventListeners.push listener

export class ModelView
    constructor: (values, @parent) ->
        @propertyListeners = {}
        @set values
        # @attach()

    scene: =>
        console.log @parent
        if @parent? then @parent.scene()

    pushEvent: (path, event) =>
        for listener in eventListeners
            listener path, event

    redraw: => @set @

    set: (values) =>
        @updateModel values
        @updateView()

    attach: =>
        console.log 'attach', @
        if @scene()? and @def?
            @view = @scene().add @def
            @group = group [@view]
            @registerEvents()
            @updateView()

    detach: =>
        if @view?
            # @scene.remove @view
            @view = null

    reatach: =>
        @detach()
        @attach()

    emitProperty: (name, property) =>
        unless @[name] == property
            @[name] = property
            if @propertyListeners[name]?
                listeners = @propertyListeners[name]
                @propertyListeners[name] = null
                for listener in listeners
                    listener property

    subscribeProperty: (name, listener) =>
        @propertyListeners[name] ?= []
        @propertyListeners[name].push listener
