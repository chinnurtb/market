local symbol = KEYS[1]

local sdata = redis.call('HMGET', symbol, 'price', 'last_volume', 'outstanding')
local bdata = redis.call('SORT', 'book:bid:limit:'..symbol, 'BY', 'order:*->limit', 'LIMIT', '0', '1', 'GET', 'order:*-iilmit', 'DESC')
local adata = redis.call('SORT', 'book:ask:limit:'..symbol, 'BY', 'order:*->limit', 'LIMIT', '0', '1', 'GET', 'order:*->limit', 'ASC')


local price=tonumber(sdata[1]) or 0
local volume=tonumber(sdata[2]) or 0
local outstanding=tonumber(sdata[3]) or 0
local bid_high=tonumber(bdata[1]) or price
local ask_low=tonumber(adata[1]) or price
return {
  price,
  volume,
  outstanding,
  bid_high,
  ask_low
}
