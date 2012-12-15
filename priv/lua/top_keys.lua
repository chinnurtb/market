local users = redis.call('SORT', 'users', 'BY', 'user:*->cash', 'LIMIT', '0', '10',  'GET', 'user:*->nickname', 'DESC')
return users
