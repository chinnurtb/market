local id = KEYS[1]
return redis.call('HMGET', 'order:'..id, 'id', 
  'user', 'symbol', 'type', 'limit', 
  'quantity', 'quantity_constraint',
  'time_in_force', 'timestamp')
