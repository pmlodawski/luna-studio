mockSocket = () ->
    addEventListener    : () -> {}
    removeEventListener : () -> {}
    send                : () -> {}

module.exports = () ->
  listenConnection = mockSocket()
  sendConnection   = mockSocket()
  isConnOpen       = false
  listeners        =
    onOpen    : []
    onMessage : []
    onClose   : []
    onError   : []

  attachListeners = () ->
    listeners.onOpen.forEach (listener) ->
      listenConnection.addEventListener("open", listener)
    listeners.onMessage.forEach (listener) ->
      listenConnection.addEventListener("message", listener)
    listeners.onClose.forEach (listener) ->
      listenConnection.addEventListener("close", listener)
    listeners.onError.forEach (listener) ->
      listenConnection.addEventListener("error", listener)

  return
    isOpen: () -> isConnOpen
    onOpen: (listener) ->
      isConnOpen = true
      listeners.onOpen.push listener
      listenConnection.addEventListener "open", listener
    onMessage: (listener) ->
      listeners.onMessage.push listener
      listenConnection.addEventListener "message", listener
    onClose: (listener) ->
      listeners.onClose.push listener
      listenConnection.addEventListener "close", listener
    onError: (listener) ->
      listeners.onError.push listener
      listenConnection.addEventListener "error", listener
    connect: (listenAddr, sendAddr) ->
      listenConnection = new WebSocket listenAddr
      listenConnection.binaryType = "arraybuffer"
      sendConnection = new WebSocket sendAddr
      sendConnection.binaryType = "arraybuffer"
      attachListeners()
    send: (data) ->
      sendConnection.send data
