import * as keypress from 'keypress.js'
import * as keymap from './keymap.cson'

export class Keyboard
  constructor: (@handlers) ->
    window.k = @
    @listener = new keypress.Listener()
    @setContext()

  setContext: (context = 'default') =>
    @listener.reset()
    handlers = @handlers?[context]
    for key, binding of keymap[context]
      handler = handlers?[binding]
      if handler?
        @listener.register_combo
          keys: key
          on_keydown: handler
