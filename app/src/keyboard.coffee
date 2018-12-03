import * as keypress from 'keypress.js'
import * as keymap from './keymap.cson'

export class Keyboard
  constructor: (@defaultHandlers) ->
    @listener = new keypress.Listener()
    @unsetContext()

  unsetContext: =>
    @setContext null, @defaultHandlers

  setContext: (context, handlers) =>
    @listener.reset()
    for key, binding of keymap[context or 'default']
      handler = handlers?[binding]
      if handler?
        @listener.register_combo
          keys: key
          on_keydown: handler
