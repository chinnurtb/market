{set_bid, [
  "local user, symbol = KEYS[1], KEYS[2]",
  "price, amount, ts = ARGS[1], ARGS[2], ARGS[3]",
  "local cash = redis.call('HGET', 'user:'..user, 'cash')",
  "if cash < price then return {'invalid', 'bid exceeds funds'} end",
  "local bid_id = redis.call('incr', 'bid:ids')",
  "redis.call('HMSET', 'bid:'..bid_id, 'user', user, 'price', price, 'amount', amount, 'ts', ts)",
  "redis.call('ZADD', 'bids:'..symbol, price, bid_id)" ]}.
