local id, symbol, side, market = KEYS[1], KEYS[2], KEYS[3], KEYS[4]

local exists = redis.call('EXISTS', 'order:'..id) or 0
if exists == 0 then
  return {'error', 'not found'}
end
redis.call('SADD', 'book:'..side..':'..market..':'..symbol, id)
redis.call('HSET', 'order:'..id, 'state', 'booked')
return true
