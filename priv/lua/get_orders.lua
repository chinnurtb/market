local side, type, symbol = KEYS[1], KEYS[2], KEYS[3]
local order_by, direction
if type == 'market' then
  order_by = 'order:*->ts'
  direction = 'ASC'
else
  order_by = 'limit'
  if side == 'buy' then
    direction = 'ASC'
  else
    direction = 'DESC'
  end
end
local book = redis.call('SORT', 'book:'..side..':'..type..':'..symbol, "BY", order_by,
        'GET', 'order:*->id', 'GET', 'order:*->user', 'GET', 'order:*->symbol',
        'GET', 'order:*->type', 'GET', 'order:*->limit', 'GET', 'order:*->quantity',
        'GET', 'order:*->quantity_constraint', 'GET', 'order:*->time_in_force',
        'GET', 'order:*->timestamp', direction)
return book
