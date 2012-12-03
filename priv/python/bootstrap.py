import redis
r = redis.StrictRedis()
r.flushdb()

for i in range(1, 100):
  ask = {
      'id':i,
      'user':0,
      'symbol':'cocaine',
      'type':'ask',
      'limit':'none',
      'quantity':100,
      'quantity_constraint':'none',
      'time_in_force':'cancelled',
      'timestamp':'123423432%d' % i
  }
  r.hmset('order:%d' % i, ask)
  r.sadd('book:ask:market:cocaine', i)
