import redis
import constants, utils

pool = redis.ConnectionPool()
scripts = {}

def load():
  import glob
  from posixpath import basename, dirname
  import os

  dirlist = glob.glob("./../../lua/*.lua")

  f = open("./../../lua/lib.lua", 'r')
  lib = f.read()
  f.close()
  r = redis.StrictRedis()

  for f in dirlist:
    proc = basename(f).split(".")[0]
    f = open(f, 'r')
    script = lib + f.read()
    f.close()
    scripts[proc] = r.register_script(script)

load()

def get_orders(side, type, symbol):
  r = redis.StrictRedis(connection_pool=pool)
  orders = scripts["get_orders"](keys=[side, type, symbol], args=[])
  return utils.zip_orders(orders)

def get_symbol_data(symbol):
  r = redis.StrictRedis(connection_pool=pool)
  ret = scripts["symbol_data"](keys=[symbol], args=[])
  return utils.zip_symbol_data(ret)
