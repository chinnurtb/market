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
    user=tonumber(o[6]),
    symbol=o[7],
    type=o[8],
    state=o[9]
  }
end

local ensure_darkpool = function(user, prop, min)
  local r = redis.call('HMGET', 'user:'..user, prop, 'dark')
  local v = tonumber(r[1])
  local diff = min - v
  if r[2] == '1' then
    --DARK POOL USER
    if diff > 0 then
      if prop == 'cash' then
        redis.call('HINCRBY', 'user:'..key, 'cash', diff)
        redis.call('INCRBY', 'cash_outstanding', diff)
      else
        redis.call('HINCRBY', 'user:'..key, prop, diff)
        redis.call('HINCRBY', prop, 'outstanding', diff)
      end
    end
  end
end
