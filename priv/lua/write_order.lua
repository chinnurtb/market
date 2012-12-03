local id, user, symbol = KEYS[1], KEYS[2], KEYS[3]
local side, limit, quantity = ARGV[1], ARGV[2], ARGV[3]
local qconst, tif, ts = ARGV[4], ARGV[5], ARGV[6]
local market = ARGV[7]

redis.call('SADD', 'book:'..side..':'..market..':'..symbol, id)
redis.call('HMSET', 'order:'..id, 'id', id,
  'user', user, 'symbol', symbol, 'type', side, 'limit', limit,
  'quantity', quantity, 'quantity_constraint', qconst,
  'time_in_force', tif, 'timestamp', ts)
redis.call('SADD', 'user:orders:'..user, id)
