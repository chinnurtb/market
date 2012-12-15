from bottle import request, HTTPError
from db import get_secret

import hashlib, hmac, datetime, time
from urllib import urlencode

def _valid():
  sig = request.forms.get('sig', None)
  key = request.forms.get('key', None)
  ts = request.forms.get('ts', None)
  if None in [sig, key, ts]:
    print "missing sig, key, or ts"
    return False
  secret = get_secret(key)
  if secret is None:
    print "cant find secret for key"
    return False
  try:
    d = (int(time.time()) - int(ts)) / 3600
    if d > 15 or d < -15:
      print "timestamp is off"
      return False
  except:
    print "error timestamp"
    return False
  keys = request.forms.keys()
  keys.remove('sig')
  keys.sort()
  values = map(request.forms.get, keys)
  s = urlencode(zip(keys, values))
  signature = hmac.new(
    key=key,
    msg=s,
    digestmod=hashlib.sha256).hexdigest()
  if signature == sig:
    return True
  return False

def auth(view):
  def wrapper(*args, **kwargs):
    if not _valid():
      raise HTTPError(403)
    return view(*args, **kwargs)
  return wrapper
