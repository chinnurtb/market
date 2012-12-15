import json
import urllib2
import urllib



def buy(key, symbol, limit, quantity, qconst, tif):
  return call({
    'method':'buy',
    'user':key,
    'symbol':symbol,
    'limit':limit,
    'quantity':quantity,
    'quantity_constraint':qconst,
    'time_in_force':tif
  })

def sell(key, symbol, limit, quantity, qconst, tif):
  return call({
    'method':'sell',
    'user':key,
    'symbol':symbol,
    'limit':limit,
    'quantity':quantity,
    'quantity_constraint':qconst,
    'time_in_force':tif
  })

def cancel(order):
  return call({'method':'cancel', 'id':order})

def call(params={}):
  try:
    url = 'http://localhost:8000/' 
    req = urllib2.Request(url, urllib.urlencode(params))
    res = urllib2.urlopen(req)
    jsn = res.read()
    try:
      return json.loads(jsn)
    except Exception, e:
      return {'result':'error','reason':'bad server response'}
  except Exception, e:
    return {'result':'error', 'reason':'market unavailable'}
