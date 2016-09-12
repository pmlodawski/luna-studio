from twython import TwythonStreamer
import socket
import sys
import thread

HOST = "localhost"
PORT = 4321

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

APP_KEY = "bSRZBrtLbDEon8L43mka4CzON"
APP_SECRET = "FNHzxjgXcJruI7ClL1dwj3YR40T3pzt8h4CJWNav4KpxMZC7mb"
ACCESS_TOKEN = "2921511033-0vRXpe8LO4MvlddeAqUzRoVhzQkT96ICPhH90du"
ACCESS_SECRET = "Tu8DZNwzt5QcUZPkPo3j1VnyClxzZb7MRf9ofPxsagQzq"

class MyStreamer(TwythonStreamer):
    def on_success(self, data):
        if 'text' in data:
            print data['text'].encode('utf-8')
            sock.sendto(data['text'].encode('utf-8'), (HOST, PORT))

    def on_error(self, status_code, data):
        print status_code
        self.disconnect()

sock.sendto("Nodelab is green", (HOST, PORT))
stream = MyStreamer(APP_KEY, APP_SECRET, ACCESS_TOKEN, ACCESS_SECRET)
stream.statuses.filter(track="#testingNodelabApp,@realDonaldTrump,@HillaryClinton,#Nodelab,#Luna,#nodelab,#luna,#TCD,#tcd,#TechCrunch,#disrupt,#Disrupt")
