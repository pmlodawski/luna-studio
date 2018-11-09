import * as path    from 'path'
import * as logger  from 'luna-logger'

import * as Backend from './backend'
import * as Enum    from './enum'


import {NodeEditor}      from './NodeEditor'

# TODO: We should never keep such functions attached to window.
window.listVisualizers = => [] #TODO
window.getInternalVisualizersPath = => '' #TODO
window.getLunaVisualizersPath = => '' #TODO
window.getInternalVisualizers = => {} #TODO
window.getLunaVisualizers = => [] #TODO
window.getProjectVisualizers = => [] #TODO
window.getImportedVisualizers = => {} #TODO

############
### Main ###
############

# TODO: This should be obtained from the backend, not hardcoded here
messages = Enum.make(
  'Init', 
  'ProjectSet', 
  'FileOpened',
  'SetProject',
  'GetBuffer'
)

main = () -> 
  backend = await Backend.initialize()

  nodeBackend = backend.node
  codeBackend = backend.code

  class LunaStudio
    launch: =>
      @projectPath = '/tmp/luna/Test'
      codeBackend.connect nodeBackend.connector
      codeBackend.onStatus @__onMessage
      nodeBackend.onNotification (msg) ->
        logger.warning 'Unhandled', msg
      codeBackend.start()

    # TODO: This design is to be changed. Having always 3 variables for 
    #       different purposes is very bad.
    __onMessage: (act, arg0, arg1) =>
      logger.group ('Received ' + act), =>
        logger.info 'args', { arg0, arg1 }
        switch act
          when messages.Init then @sendMessage
              tag: messages.SetProject
              _path: @projectPath
          when messages.ProjectSet then @openMain()
          when messages.FileOpened then @sendMessage
              tag: messages.GetBuffer
              _path: arg0

    sendMessage: (msg) -> 
      logger.info 'Sending', msg
      codeBackend.pushInternalEvent msg

    openMain: => logger.group 'Launching Node Editor', =>
      mainLocation = path.join @projectPath, 'src', 'Main.luna'
      # TODO: NodeEditor should open and initialize immediately.
      #       New file should be provided on demand. Initialization of 
      #       WebGL etc when openning file is a very bad idea.
      @nodeEditor ?= new NodeEditor mainLocation, nodeBackend, codeBackend

  logger.group 'launching Luna Studio', =>
    ls = new LunaStudio
    ls.launch()

  # n.start();
  # callback.connector
  # callback.setNodeEditorView
  # callback.onNotification
  # callback.pushEvent
  # callback.view.pushEvent
  # callback.setEventFilter
  # callback.onExpectedEvent



main()
