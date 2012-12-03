local side, type, symbol = KEYS[1], KEYS[2], KEYS[3]
local book = redis.call('SORT', 'book:'..side..':'..type..':'..symbol, "BY", "order:*->timestamp",
        'GET', 'order:*->id', 'GET', 'order:*->user', 'GET', 'order:*->symbol',
        'GET', 'order:*->type', 'GET', 'order:*->limit', 'GET', 'order:*->quantity',
        'GET', 'order:*->quantity_constraint', 'GET', 'order:*->time_in_force',
        'GET', 'order:*->timestamp', 'ASC')
return book
