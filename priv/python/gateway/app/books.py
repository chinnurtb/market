from copy import deepcopy
from db import get_orders
import constants, events, utils
import gevent

books = {}
def load():
  for symbol in constants.SYMBOLS:
    books[symbol] = {
      'market':{
        'bid':get_orders('bid', 'market', symbol),
        'ask':get_orders('ask', 'market', symbol)
      },
      'limit':{
        'bid':get_orders('bid', 'limit', symbol),
        'ask':get_orders('ask', 'limit', symbol)
      }
    }
    

def book_listener():
  while True:
    try:
      events.book.wait()
      order = events.book_cache[-1]
      if order.limit == 'none':
        market = 'market'
      else:
        market = 'limit'
      symbol = order.symbol
      type = order.type
      books[symbol][market][type].append(order)
    except: pass

def cancel_listener():
  while True:
    try:
      events.cancel.wait()
      order = events.cancel_cache[-1]
      remove_order(order)
    except: pass

def txn_listener():
  while True:
    try:
      events.txn.wait()
      txn = events.txn_cache[-1]
      remove_order(txn['buy'])
      remove_order(txn['sell'])
    except Exception, e: print e

def run():
  load()
  procs = [
    gevent.spawn(book_listener),
    gevent.spawn(cancel_listener),
    gevent.spawn(txn_listener)
  ]
  gevent.joinall(procs)
  

def get():
  return deepcopy(books)

def remove_order(order):
  if order['limit'] == 'none':
    market = 'market'
  else:
    market = 'limit'
  symbol = order['symbol']
  type = order['type']
  l =  books[symbol][market][type]
  l[:] = [o for o in l if o['id'] == order['id']]
  books[symbol][market][type] = l
