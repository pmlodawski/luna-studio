import {eventDispatcherMixin}   from 'basegl/event/EventDispatcher'
import {group}                  from 'basegl/display/Symbol'
import {Composable, fieldMixin} from "basegl/object/Property"

eventListeners = []

export subscribeEvents = (listener) =>
    eventListeners.push listener

export class Component extends Composable
    cons: (values, @parent) ->
        @mixin eventDispatcherMixin, @
        @propertyListeners = {}
        @set values
        # @attach()

    scene: => @parent.scene() if @parent?

    pushEvent: (path, event) =>
        for listener in eventListeners
            listener path, event

    redraw: => @set @

    set: (values) =>
        @updateModel values
        @updateView()

    attach: =>
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
            propertyEvent = new CustomEvent name, value: property
            @dispatchEvent propertyEvent if @dispatchEvent?
