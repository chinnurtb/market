local id, symbol, side, market = KEYS[1], KEYS[2], KEYS[3], KEYS[4]

redis.call('SADD', 'book:'..side..':'..market..':'..symbol, id)
redis.call('HSET', 'order:'..id, 'state', 'booked')
