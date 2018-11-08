import 'setimmediate'
import * as path from 'path'

import * as analytics    from './analytics'
import * as callback     from './callback'
import * as codeCallback from './codeCallback'
import * as config       from './config'
import * as gzip         from './gzip'
import * as uuid         from './uuid'
import * as websocket    from './websocket'
import * as Promise      from 'bluebird'

import {NodeEditor}      from './NodeEditor'


window.listVisualizers = => [] #TODO
window.getInternalVisualizersPath = => '' #TODO
window.getLunaVisualizersPath = => '' #TODO
window.getInternalVisualizers = => {} #TODO
window.getLunaVisualizers = => [] #TODO
window.getProjectVisualizers = => [] #TODO
window.getImportedVisualizers = => {} #TODO

init = websocket: websocket()
generateUUID = uuid.generateUUID
atomCallback = callback
atomCallbackTextEditor = codeCallback


###################
### Libs Config ###
###################

libsConfig = 
  'lib/node-editor.js': 
    analytics    : analytics
    atomCallback : callback
    config       : config
    gzip         : gzip
    init         : init

  'lib/text-editor.js': 
    analytics              : analytics
    atomCallbackTextEditor : codeCallback
    config                 : config
    gzip                   : gzip
    init                   : init



####################
### Libs Loading ###
####################


ajaxGetAsync = (url) ->
  return new Promise (resolve, reject) ->
    console.log ('Downloading ' + url)
    xhr = new XMLHttpRequest
    xhr.timeout = 5000
    xhr.onreadystatechange = (evt) ->
      if (xhr.readyState == 4)
        if (xhr.status == 200)
          resolve 
            url  : url 
            text : xhr.responseText 
        else 
          reject (throw new Error xhr.statusText)
    xhr.onprogress = (evt) ->
       progress = Math.floor(evt.loaded / evt.total * 1000) / 10
       console.log ('Progress ' + progress + '%')
    xhr.addEventListener "error", reject
    xhr.open 'GET', url, true
    xhr.send null

fileNames = Object.keys(libsConfig)
loader    = Promise.map fileNames, (fileName) -> ajaxGetAsync fileName
loader.catch (e) -> console.error "ERROR loading scripts!"

loadLibs = () ->
  srcs = await loader
  fns  = {}
  for src in srcs
    console.log('Compiling ' + src.url)
    args   = libsConfig[src.url]
    argMap = '{' + Object.keys(args).join(",") + '}'
    fn = new Function argMap, src.text
    fns[src.url] = (() -> fn(args))
  fns


############
### Main ###
############

main = () -> 
  fns = await loadLibs()

  globalRegistry = {}

  nodeBackend =
      start:           fns['lib/node-editor.js']
      connector:       callback.connector
      setView:         callback.setNodeEditorView
      onNotification:  callback.onNotification
      pushEvent:       callback.pushEvent
      pushViewEvent:   callback.view.pushEvent
      setEventFilter:  callback.setEventFilter
      onExpectedEvent: callback.onExpectedEvent

  codeBackend =
      start:               fns['lib/text-editor.js']
      connect:             (connector)   => connector(globalRegistry)
      lex:                 (stack, data) => codeCallback.lex stack, data
      onInsertCode:        (callback)    => codeCallback.onInsertCode callback
      onInterpreterUpdate: (callback)    => codeCallback.onInterpreterUpdate callback
      onSetBuffer:         (callback)    => codeCallback.onSetBuffer callback
      onSetClipboard:      (callback)    => codeCallback.onSetClipboard callback
      pushDiffs:           (diffs)       => codeCallback.pushDiffs diffs
      pushInternalEvent:   (data)        => codeCallback.pushInternalEvent data
      onStatus:            (callback)    => codeCallback.onStatus callback

  class LunaStudio
      launch: =>
          @projectPath = '/tmp/luna/Test'
          codeBackend.connect nodeBackend.connector
          codeBackend.onStatus @__onStatus
          nodeBackend.onNotification console.log
          codeBackend.start()

      __onStatus: (act, arg0, arg1) =>
          console.log 'status:', { act, arg0, arg1 }
          switch act
              when 'Init'
                  codeBackend.pushInternalEvent
                      tag: 'SetProject'
                      _path: @projectPath
              when 'ProjectSet'
                  @openMain()
              when 'FileOpened'
                  codeBackend.pushInternalEvent
                      tag: 'GetBuffer'
                      _path: arg0

      openMain: =>
          mainLocation = path.join @projectPath, 'src', 'Main.luna'
          @nodeEditor ?= new NodeEditor mainLocation, nodeBackend, codeBackend


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
