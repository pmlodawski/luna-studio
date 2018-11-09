import * as logger       from 'luna-logger'
import * as Promise      from 'bluebird'

####################
### Libs Loading ###
####################

ajaxGetAsync = (el, progress) ->
  return new Promise (resolve, reject) ->
    logger.info 'Downloading', el.id
    xhr = new XMLHttpRequest
    xhr.timeout = 5000
    xhr.onreadystatechange = (evt) ->
      if (xhr.readyState == 4)
        if (xhr.status == 200)
          resolve
            id   : el.id
            text : xhr.responseText
        else
          reject (throw new Error xhr.statusText)
    xhr.onprogress = (evt) -> progress
      id     : el.id
      loaded : evt.loaded
    xhr.addEventListener "error", reject
    xhr.open 'GET', el.url, true
    xhr.send null

export load = (config) -> logger.group 'Loading libs', =>
  loader = Promise.map Object.keys(config), (lib) ->
    ajaxGetAsync {id: lib, url: config[lib].path}, () ->
  loader.catch (e) -> console.error "ERROR loading scripts!"
  srcs = await loader
  fns  = {}
  srcs.forEach (src) ->
    logger.info 'Compiling', src.id
    lib      = config[src.id]
    args     = lib.args
    argNames = Object.keys(args)
    argMap   = '{' + argNames.join(",") + '}'
    fn = new Function argMap, src.text
    fns[src.id] = (() -> fn(args))
  fns

