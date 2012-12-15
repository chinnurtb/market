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

def get_secret(key):
  r = redis.StrictRedis(connection_pool=pool)
  return r.hget('user:%s' % key, 'secret')

def is_dark(key):
  r = redis.StrictRedis(conection_pool=pool)
  return r.hget('user:%s' % key, 'dark') == '1'

def get_inventory(key):
  r = redis.StrictRedis(connection_pool=pool)
  user = r.hgetall('user:%s' % key)
  if user:
    try:
      del user['key']
      del user['secret']
      del user['dark']
    except: pass
  return user
