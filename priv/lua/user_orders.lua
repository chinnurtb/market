local user = KEYS[1]

local orders = redis.call('SMEMBERS', 'user:orders:'..user)
local ret = {}
for i,v in ipairs(orders) do
  local o = redis.call('HGETALL', 'order:'..v)
  table.insert(ret, o)
end
return ret
