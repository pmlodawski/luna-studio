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
    logger.group 'Launching code backend', =>
      @backend.code.start()
    logger.group 'Creating node editor', =>
      @nodeEditor = new NodeEditor @, @backend

  openProjectDialog: =>
    projectPath = window.prompt "Enter project location", "/tmp/luna/Test"
    @setProject projectPath

  # TODO: This design is to be changed. Having always 3 variables for
  #       different purposes is very bad.
  __onMessage: (act, arg0, arg1) =>
    logger.group ('Received ' + act), =>
      logger.info 'args', { arg0, arg1 }
      switch act
        when messages.Init then @__onInit()
        when messages.ProjectSet then @__onProjectSet()
        when messages.FileOpened then @__onFileOpened arg0

  __onInit: =>
    @setProject @projectPath

  setProject: (@projectPath) =>
      @backend.code.pushInternalEvent
        tag: messages.SetProject
        _path: @projectPath

  __onFileOpened: (filePath) =>
    @nodeEditor.setFile filePath
    @backend.code.pushInternalEvent
      tag: messages.GetBuffer
      _path: filePath

  __onProjectSet: => logger.group 'Opening Main.luna', =>
    filePath = path.join @projectPath, 'src', 'Main.luna'
    @backend.code.pushInternalEvent
      tag: "OpenFile"
      _path: filePath
