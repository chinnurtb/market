import gevent
from gevent import monkey
from gevent.event import Event
monkey.patch_all()

import msgpack
import redis
import utils

book = Event()
book_cache = []
cancel = Event()
cancel_cache = []
txn = Event()
txn_cache = []

def market_events():
  r = redis.StrictRedis()
  ps = r.pubsub()
  ps.subscribe(["book", "cancel", "txn"])
  for msg in ps.listen():
    try:
      if msg['type']== 'message':
        data = msgpack.unpackb(msg['data'], use_list=True)
        ev = msg['channel']
        if ev == "book":
          add_to_cache(book_cache, data)
          book.set()
          book.clear()
        if ev == "cancel":
          add_to_cache(cancel_cache,  data)
          cancel.set()
          cancel.clear()
        if ev == "txn":
          add_to_cache(txn_cache, data)
          txn.set()
          txn.clear()

    except Exception, e:
      print "error unpacking"
      print e

def add_to_cache(cache, item):
  cache.append(item)
  if len(cache) > 10:
    del cache[:10]

if __name__ == "__main__":
  c = gevent.spawn(market_events)
  gevent.joinall([c])
