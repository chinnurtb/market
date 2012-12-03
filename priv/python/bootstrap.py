import redis
r = redis.StrictRedis()
r.flushdb()
for i in range(1, 100000):
  for x in ['cocaine', 'pot', 'speed']:
    r.hset('user:%d' % i, x, 1000)
  r.hset('user:%d' % i, 'cash', 10000)
  r.sadd('users', i)
