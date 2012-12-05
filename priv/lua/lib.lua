local log = function(msg)
  redis.log(redis.LOG_DEBUG, msg)
end
local get_order = function(id)
  local o = redis.call('HMGET','order:'..id,
    'limit', 'quantity', 'quantity_constraint',
    'tif', 'ts', 'user', 'symbol', 'type')
  return {
    id=id,
    limit=o[1],
    quantity=o[2],
    quantity_constraint=o[3],
    tif=o[4],
    ts=o[5],
    user=o[6],
    symbol=o[7],
    type=o[8]
  }
end
