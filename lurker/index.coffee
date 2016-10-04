Twitter = require('twitter')
_ = require 'underscore'

APP_KEY = "bSRZBrtLbDEon8L43mka4CzON"
APP_SECRET = "FNHzxjgXcJruI7ClL1dwj3YR40T3pzt8h4CJWNav4KpxMZC7mb"
ACCESS_TOKEN = "2921511033-0vRXpe8LO4MvlddeAqUzRoVhzQkT96ICPhH90du"
ACCESS_SECRET = "Tu8DZNwzt5QcUZPkPo3j1VnyClxzZb7MRf9ofPxsagQzq"

client = new Twitter
  consumer_key: APP_KEY
  consumer_secret: APP_SECRET
  access_token_key: ACCESS_TOKEN
  access_token_secret: ACCESS_SECRET


net = require('net')
sockets = []
port = 9991

globalTopics = []
oldTopics = ""

getTopics = ->
  allTopics = []
  sockets.forEach (socket) -> allTopics = allTopics.concat(socket.topics)
  _.uniq(allTopics)

initializeStream = ->
  globalTopics = _.uniq(globalTopics)
  if globalTopics.length > 100
    globalTopics = getTopics()
  newTopics = globalTopics.join(',')
  if oldTopics != newTopics
    if global.stream
      global.stream.destroy()
      console.log("DESTRONY STERAM")
      setTimeout (-> startStream(newTopics)), 1000
    else
      startStream(newTopics)

startStream = (newTopics) ->
  oldTopics = newTopics
  console.log("ALLTOPICS:", newTopics)
  if globalTopics.length > 0
    true
    global.stream = client.stream('statuses/filter', {track: newTopics})
    global.stream.on 'data', (event) ->
      if event && event.text
        broadcast event.text
        console.log(event && event.text);
    global.stream.on 'error', (error) -> console.error(error)
  else
    global.stream = null


server = net.createServer((socket) ->
  sockets.push socket
  socket.topics = []
  socket.on 'data', (data) ->
    socket.topics = data.toString().trim().split(",")
    globalTopics = globalTopics.concat(socket.topics)
    initializeStream()
  socket.on 'end', ->
    removeSocket socket
    initializeStream()
  socket.on 'error', (error) -> console.log 'Socket got problems: ', error.message
)

broadcast = (message) ->
  sockets.forEach (socket) ->
    shouldSend = false
    socket.topics.forEach (topic) ->
      if message.indexOf(topic) >= 0
        shouldSend = true
    socket.write message + "\n" if shouldSend

removeSocket = (socket) ->
  sockets.splice sockets.indexOf(socket), 1

server.on 'error', (error) -> console.log 'So we got problems!', error.message

server.listen port, ->
  console.log 'Server listening at tcp://localhost:' + port
