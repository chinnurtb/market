import gevent
from gevent_zeromq import zmq
import msgpack

context = zmq.Context()

def listener():
  socket = context.socket(zmq.SUB)
  socket.connect("tcp://127.0.0.1:5560")
  socket.setsockopt(zmq.SUBSCRIBE, "")
  
  while True:
    msg = socket.recv()
    try:
      event = msgpack.unpackb(msg)
      print event
    except:
      print "error unpacking"
      print msg

if __name__ == "__main__":
  c = gevent.spawn(listener)
  gevent.joinall([c])
