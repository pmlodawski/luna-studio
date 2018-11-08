import 'setimmediate'

import * as analytics    from './analytics'
import * as callback     from './callback'
import * as codeCallback from './codeCallback'
import * as config       from './config'
import * as websocket    from './websocket'
import * as uuid         from './uuid'
import * as Promise      from 'bluebird'

# init = websocket: websocket()
# generateUUID = uuid.generateUUID
# atomCallback = callback
# n = nodeEditor({analytics, atomCallback, config, generateUUID, init})
# atomCallbackTextEditor = codeCallback
# c = codeEditor({analytics, atomCallbackTextEditor, config, init})

# n.start()
# callback.connector
# callback.setNodeEditorView
# callback.onNotification
# callback.pushEvent
# callback.view.pushEvent
# callback.setEventFilter
# callback.onExpectedEvent


###################
### Libs Config ###
###################

libsConfig = 
  'lib/node-editor.js': 
    analytics    : analytics
    atomCallback : callback
    config       : config
    generateUUID : generateUUID
    init         : websocket: websocket()



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
loader.then (srcs) ->
  for src in srcs
    console.log('Compiling ' + src.url)
    args   = libsConfig[src.url]
    argMap = '{' + Object.keys(args).join(",") + '}'
    fn = new Function argMap, src.text
    console.log('Evaluating ' + src.url)
    fn(args)
