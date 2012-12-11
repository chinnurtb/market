local tx = KEYS[1]

local ret = redis.call('HMGET', 'tx:'..tx, 'buy', 'sell', 'price', 'quantity')
local buy = get_order(ret[1])
local sell = get_order(ret[2])
local price = tonumber(ret[3])
local quantity = tonumber(ret[4])
local market

if buy.limit == 'none' then market = "market" else market = "limit" end
redis.call('SREM', 'user:orders:'..buy.user, buy.id)
redis.call('SREM', 'book:bid:'..market..':'..market..':'..buy.symbol, buy.id)
redis.call('HSET', 'order:'..buy.id, 'state', 'closed')
redis.call('SADD', 'order:'..buy.id..':contras', tx)
redis.call('LPUSH', 'user:orders:'..buy.user..':closed', buy.id)
local expired = redis.call('LRANGE', 'user:orders:'..buy.user..':closed', 1000, -1)
redis.call('LTRIM', 'user:orders:'..buy.user..':closed', 0, 999)
for k,v in ipairs(expired) do
  redis.call('DEL', 'order:'..v)
  redis.call('SREM', 'user:orders:'..buy.user..':closed', v)
  redis.call('DEL', 'order:'..v..':contras')
end

if sell.limit == 'none' then market = "market" else market = "limit" end
redis.call('SREM', 'user:orders:'..sell.user, sell.id)
redis.call('SREM', 'book:ask:'..market..':'..sell.symbol, sell.id)
redis.call('HSET', 'order:'..sell.id, 'state', 'closed')
redis.call('SADD', 'order:'..sell.id..':contras', tx)
redis.call('LPUSH', 'user:orders:'..sell.user..':closed', sell.id)
local expired = redis.call('LRANGE', 'user:orders:'..sell.user..':closed', 1000, -1)
redis.call('LTRIM', 'user:orders:'..sell.user..':closed', 0, 999)
for k,v in ipairs(expired) do
  redis.call('DEL', 'order:'..v)
  redis.call('SREM', 'user:orders:'..sell.user..':closed', v)
  redis.call('DEL', 'order:'..v..':contras')
end
redis.call('HMSET', buy.symbol, 'price', ret[3], 'last_volume', ret[4])

local set_stats = function(symbol, type, price, quantity, ts)
  redis.call('HINCRBY', symbol..':stats:'..type, ts..'_volume', quantity)
  local pret = redis.call('HMGET', symbol..':stats:'..type, ts..'_price_high', ts..'_price_low')

  local high = tonumber(pret[1]) or 0
  local low = tonumber(pret[2]) or 0
  if price > high then
    redis.call('HSET', symbol..':stats:'..type, ts..'_price_high', price)
  end
  if price < low or low == 0 then
    redis.call('HSET', symbol..':stats:'..type, ts..'_price_low', price)
  end
  redis.call('SADD', symbol..':'..type, ts)
end

local ts = tonumber(ARGV[1])

local minute = math.floor(ts/60)*60
local hour = math.floor(ts/3600)*3600
local day = math.floor(ts/(3600*24))*(3600*24)

set_stats(buy.symbol, 'minute', price, quantity, minute)
set_stats(buy.symbol, 'hour', price, quantity, hour)
set_stats(buy.symbol, 'day', price, quantity, day)
