local log = function(msg)
  redis.log(redis.LOG_DEBUG, msg)
end
local get_order = function(id)
  local o = redis.call('HMGET','order:'..id,
    'limit', 'quantity', 'quantity_constraint',
    'time_in_force', 'timestamp', 'user', 'symbol', 'type', 'state')
  return {
    id=id,
    limit=o[1],
    quantity=o[2],
    quantity_constraint=o[3],
    tif=o[4],
    ts=o[5],
    user=o[6],
    symbol=o[7],
    type=o[8],
    state=o[9]
  }
end
