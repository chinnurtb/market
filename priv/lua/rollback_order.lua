local txid = KEYS[1]

local ret = redis.call('HMGET', 'tx:'..txid, 'buy', 'sell', 'price', 'quantity')

local tx = {
  id=txid,
  buy=ret[1],
  sell=ret[2],
  price=ret[3],
  quantity=ret[4]
}

local tx_cost = tx.price * tx.quantity

local buy = get_order(tx.buy)
local sell = get_order(tx.sell)

--RESET LOCK
redis.call('HDEL', 'order:'..tx.buy, 'lock')
redis.call('HDEL', 'order:'..tx.sell, 'lock')

--UNDO TRANSACTION
redis.call('HINCRBY', 'user:'..buy.user, buy.symbol, tx.quantity * (-1))
redis.call('HINCRBY', 'user:'..buy.user, 'cash', tx_cost)
redis.call('HINCRBY', 'user:'..sell.user, buy.symbol, tx.quantity)
redis.call('HINCRBY', 'user:'..sell.user, 'cash', tx_cost * (-1))
redis.call('DEL', 'tx:'..txid)
redis.call('SREM', 'user:transactions:'..buy.user, txid)
redis.call('SREM', 'user:transactions:'..sell.user, txid)
