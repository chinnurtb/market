local order_id, contra_id = KEYS[1], KEYS[2]

local order = {
  id=order_id,
  limit=ARGV[1],
  quantity=ARGV[2],
  quantity_constraint=ARGV[3],
  tif=ARGV[4],
  ts=ARGV[5],
  user=ARGV[6],
  symbol=ARGV[7],
  type=ARGV[8]
}

local contra = {
  id=contra_id,
  limit=ARGV[9],
  quantity=ARGV[10],
  quantity_constraint=ARGV[11],
  tif=ARGV[12],
  ts=ARGV[13],
  user=ARGV[14],
  symbol=ARGV[15],
  type=ARGV[16]
}

local quantity, price, lock = tonumber(ARGV[17]), tonumber(ARGV[18]), ARGV[19]
local buy, sell

if order.type == 'bid' then
  buy = order
  sell = contra
else
  buy = contra
  sell = order
end

local tx_cost = price * quantity

local cash = tonumber(redis.call('HGET', 'user:'..buy.user, 'cash')) or 0
local holdings = tonumber(redis.call('HGET', 'user:'..sell.user, buy.symbol)) or 0

local buy_lock = redis.call('HGET', 'order:'..buy.id, 'lock') or nil
local sell_lock = redis.call('HGET', 'order:'..sell.id, 'lock') or nil
log(buy_lock)
log(sell_lock)
log(lock)

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
