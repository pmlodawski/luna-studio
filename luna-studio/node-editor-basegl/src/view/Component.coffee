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

    withScene: (fun) => @parent.withScene fun if @parent?

    pushEvent: (path, event) =>
        for listener in eventListeners
            listener path, event

    redraw: => @set @

    set: (values) =>
        @updateModel values
        if @view?
            @updateView()

    attach: => @withScene (scene) =>
        if @def?
            if @def instanceof Array
                @view = {}
                views = []
                for def in @def
                    @view[def.name] = scene.add def.def
                    views.push @view[def.name]
                @group = group views
            else
                @view = scene.add @def
                @group = group [@view]
            @updateView()
        @registerEvents?()

    detach: => withScene (scene) =>
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
