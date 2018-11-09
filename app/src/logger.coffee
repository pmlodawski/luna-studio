
####################
### Default tags ###
####################

export tags =
  info    : 'info'
  warning : 'warning'
  error   : 'error'
  


###########
### Log ###
###########

export class Log 
  constructor: (tags=[]) -> 
    @tags = new Set tags
    @time = Date.now()
  hasTag: (tag) -> @tags.has tag  

export class Message extends Log
  constructor: (tags, @msgs) ->
    super(tags)

export class GroupStart extends Log
  constructor: (tags, @msg) ->
    super(tags)

export class GroupEnd extends Log



##############
### Logger ###
##############

export class Logger 
  constructor: () ->
    @_streams = new Set

  attachStream: (stream) ->
    @_streams.add (stream)

  addLog: (log) => 
    @_streams.forEach (stream) ->
      stream.addLog (log)

  groupWith_: (ts, msg, f) =>
    @addLog (new GroupStart ts, msg)
    out = f()
    @addLog (new GroupEnd)
    out

  asyncGroupWith_: (ts, msg, f) =>
    @addLog (new GroupStart ts, msg)
    out = await f()
    @addLog (new GroupEnd)
    out

  groupWith: (ts) => (msg, f) =>
    if f.constructor.name == 'AsyncFunction'
      @asyncGroupWith_ ts, msg, f
    else
      @groupWith_ ts, msg, f

  log: (ts) => (msgs...) =>
    obj = new Message ts, msgs
    @addLog obj



###############
### Streams ###
###############

export class Stream
  constructor: () ->
    @_formatter = (a) -> a

  setFormatter: (f) => @_formatter = f
  getFormatter:     => @_formatter
  format:       (l) => @_formatter l

export class Console extends Stream
  addLog: (log) => 
    flog = @format log
    switch flog.constructor
      when GroupStart then console.group flog.msg
      when GroupEnd   then console.groupEnd()
      when Message
        print = console.log
        if flog.hasTag tags.error
          print = console.error
        else if flog.hasTag tags.warning
          print = console.warn
        print flog.msgs...



###########################
### Stream Transformers ###
###########################

export class StreamTransformer
  constructor: (@stream) ->

  addLog: (log) =>
    @stream.addLog log

  setFormatter: (f) => @stream.setFormatter f
  getFormatter:     => @stream.getFormatter()
  format:       (l) => @stream.format(l)


export class Buffered extends StreamTransformer
  constructor: (args...) ->
    super(args...)
    @_buffer = []

  addLog: (log) ->
    @_buffer.push log
    if log.hasTag tags.error then @flush()

  flush: () =>
    @_buffer.forEach (log) => @stream.addLog log
    @_buffer = []



################
### Defaults ###
################

export defaultLogger = new Logger()
defaultLogger.attachStream (new Console)

export log     = defaultLogger.log
export group   = defaultLogger.groupWith []
export info    = log [tags.info]
export warning = log [tags.warning]
export error   = log [tags.error]


