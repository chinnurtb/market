local order_id, contra_id = KEYS[1], KEYS[2]
local exists = redis.call('EXISTS', 'order:'..order_id)
if exists == '0' then
  return {'cancelled', 'not found', order_id }
end
exists = redis.call('EXISTS', 'order:'..contra_id)
if exists == '0' then
  return {'cancelled', 'not found', contra_id }
end

local order = get_order(order_id)
local contra = get_order(contra_id)

local quantity, price, lock = tonumber(ARGV[1]), tonumber(ARGV[2]), ARGV[3]
local buy, sell

if order.type == 'bid' then
  buy = order
  sell = contra
else
  buy = contra
  sell = order
end

local tx_cost = price * quantity

--MAKE SURE OUR INTERNAL BOTS
--CAN ALWAYS CLOSE, AND OUTSTANDING
--STAYS TRUE
ensure_darkpool(buy.user, 'cash', tx_cost)
ensure_darkpool(sell.user, buy.symbol, quantity)

local cash = tonumber(redis.call('HGET', 'user:'..buy.user, 'cash')) or 0
local holdings = tonumber(redis.call('HGET', 'user:'..sell.user, buy.symbol)) or 0

local buy_lock = redis.call('HGET', 'order:'..buy.id, 'lock') or nil
local sell_lock = redis.call('HGET', 'order:'..sell.id, 'lock') or nil

if buy_lock ~= nil and buy_lock ~= lock then
  return {'cancelled', 'locked', buy.id}
end
if sell_lock ~= nil and sell_lock ~= lock then
  return {'cancelled', 'locked', sell.id}
end

if tx_cost > cash then
  return {'cancelled', 'nsf', buy.id}
else
  if quantity > holdings then
    return {'cancelled', 'nsf', sell.id}
  else
    local tx_id = redis.call('INCR', 'tx:ids')
    log("being transaction "..tx_id)
    redis.call('HINCRBY', 'user:'..buy.user, buy.symbol, quantity)
    redis.call('HINCRBY', 'user:'..buy.user, 'cash', tx_cost * (-1))
    redis.call('HINCRBY', 'user:'..sell.user, buy.symbol, quantity*(-1))
    redis.call('HINCRBY', 'user:'..sell.user, 'cash', tx_cost)
    redis.call('HMSET', 'tx:'..tx_id, 'sell', sell.id, 'buy', buy.id,
                'quantity', quantity, 'price', price)
    redis.call('SADD', 'user:transactions:'..buy.user, tx_id)
    redis.call('SADD', 'user:transactions:'..sell.user, tx_id)
    redis.call('HSET', 'order:'..buy.id, 'lock', lock)
    redis.call('HSET', 'order:'..sell.id, 'lock', lock)
    log("transaction "..tx_id.." complete")
    return {tx_id, quantity, price}
  end
end
