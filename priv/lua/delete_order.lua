local id = KEYS[1]

local ret = redis.call('HMGET', 'order:'..id, 'user', 'symbol', 'type', 'limit')
if ret[1] == nil then return end
local market
if ret[4] == 'none' then market = 'market' else market = 'limit' end
redis.call('SREM', 'user:orders:'..ret[1], id)
redis.call('SREM', 'book:'..ret[3]..':'..market..':'..ret[2], ret[1], id)
redis.call('DEL', 'order:'..id)
