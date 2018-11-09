import * as logger  from 'luna-logger'
import * as path    from 'path'

import * as Enum    from './enum'

import {NodeEditor} from './NodeEditor'


# TODO: This should be obtained from the backend, not hardcoded here
messages = Enum.make(
  'Init',
  'ProjectSet',
  'FileOpened',
  'SetProject',
  'GetBuffer'
)

export class LunaStudio
  constructor: (@backend) ->

  launch: =>
    @projectPath = '/tmp/luna/Test'
    @backend.code.connect @backend.node.connector
    @backend.code.onStatus @__onMessage
    @backend.node.onNotification (msg) ->
      logger.warning 'Unhandled', msg
    logger.group 'Launching code backend', =>
      @backend.code.start()
    logger.group 'Creating node editor', =>
      @nodeEditor = new NodeEditor @backend

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
    @backend.code.pushInternalEvent msg

  openMain: => logger.group 'Opening Main.luna', =>
    mainLocation = path.join @projectPath, 'src', 'Main.luna'
    # TODO: NodeEditor should open and initialize immediately.
    #       New file should be provided on demand. Initialization of
    #       WebGL etc when openning file is a very bad idea.
