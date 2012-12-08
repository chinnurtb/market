local tx = KEYS[1]

local ret = redis.call('HMGET', 'tx:'..tx, 'buy', 'sell', 'price', 'quantity')
local buy = get_order(ret[1])
local sell = get_order(ret[2])
local market

if buy.limit == 'none' then market = "market" else market = "limit" end
redis.call('SREM', 'user:orders:'..buy.user, buy.id)
redis.call('SREM', 'book:'..buy.symbol..':'..market..':bid', buy.id)
redis.call('HSET', 'order:'..buy.id, 'state', 'closed')
redis.call('SADD', 'order:'..buy.id..':contras', tx)
redis.call('LPUSH', 'user:orders:'..buy.user..':closed', buy.id)
local expired = redis.call('LRANGE', 'user:orders:'..buy.user..':closed', 1000, -1)
redis.call('LTRIM', 'user:orders:'..buy.user..':closed', 0, 999)
for k,v in ipairs(expired) do
  redis.call('DEL', 'order:'..v)
end

if sell.limit == 'none' then market = "market" else market = "limit" end
redis.call('SREM', 'user:orders:'..sell.user, sell.id)
redis.call('SREM', 'book:'..sell.symbol..':'..market..':ask', sell.id)
redis.call('HSET', 'order:'..sell.id, 'state', 'closed')
redis.call('SADD', 'order:'..sell.id..':contras', tx)
redis.call('LPUSH', 'user:orders:'..sell.user..':closed', sell.id)
local expired = redis.call('LRANGE', 'user:orders:'..sell.user..':closed', 1000, -1)
redis.call('LTRIM', 'user:orders:'..sell.user..':closed', 0, 999)
for k,v in ipairs(expired) do
  redis.call('DEL', 'order:'..v)
end
