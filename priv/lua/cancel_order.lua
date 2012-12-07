local id = KEYS[1]
local reason = ARGV[1]
local ret = redis.call('HMGET', 'order:'..id, 'user', 'symbol', 'type', 'limit')
local market
if ret[4] == 'none' then market = 'market' else market = 'limit' end
redis.call('SREM', 'user:orders:'..ret[1], id)
redis.call('SREM', 'book:'..ret[3]..':'..market..':'..ret[2], ret[1], id)
redis.call('HMSET', 'order:'..id, 'status', 'cancelled', 'notes', reason)
redis.call('LPUSH', 'user:orders:cancelled', id)
local expired = redis.call('LRANGE', 'user:orders:cancelled', 1000, -1)
redis.call('LTRIM', 'user:orders:cancelled', 0, 999)
for k,v in ipairs(expired) do
  redis.call('DEL', 'order:'..v)
end
