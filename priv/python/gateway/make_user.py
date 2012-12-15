#from app.db import new_user

import os
import hashlib
import base64
import random
import redis
import sys


def gen_key():
  key = str(os.urandom(256))
  key = hashlib.sha256(key).hexdigest()
  return base64.b64encode(key,
      random.choice(['rA','aZ','gQ','hH','hG','aR','DD'])).rstrip('==')

def gen_secret():
  secret = str(os.urandom(1024))
  secret = hashlib.sha256(secret).hexdigest()
  return base64.b64encode(secret, 
      random.choice(['rA','aZ','gQ','hH','hG','aR','DD'])).rstrip('==')


if __name__ == "__main__":
  try:
    nickname = sys.argv[1]
  except:
    print "usage: make_user.py [nickname]"
    exit(1)
  print "making %s" % nickname
  user = {
      'key':gen_key(),
      'secret':gen_secret(),
      'cash':10000,
      'nickname':nickname
  }
  r = redis.StrictRedis()
  r.hmset('user:{0}'.format(user['key']), user)
  r.sadd('users', user['key'])
  r.incrby('cash_outstanding', by=10000)
  print user
