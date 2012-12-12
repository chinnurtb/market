from copy import deepcopy
from db import get_symbol_data
import constants, events, utils
import gevent

quotes = {}

def load():
  for symbol in constants.SYMBOLS:
    quotes[symbol] = get_symbol_data(symbol)

def timer():
  while True:
    try:
      load()
    except Exception, e:
      print e
    gevent.sleep(10)

def run():
  load()
  gevent.joinall([gevent.spawn(timer)])

def get(symbol): return deepcopy(quotes[symbol])
