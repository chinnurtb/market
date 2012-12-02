local order = make_order(KEYS[1], KEYS[2], KEYS[3], 0, ARGV[1], ARGV[2], ARGV[3], ARGV[4])
if not can_place_order(order) then return {'rejected', {{'reason', 'Existing Order'}}}; end
local counter_type
if  order.type == 'sell' then
  counter_type = 'buy'
else
  counter_type = 'sell'
end
--TRY TO MATCH A MARKET SELL ORDER
local matches = {}
local market = redis.call('ZRANGE', 'book:'..counter_type..':market:'..order.symbol, 0, -1)
for k,v in ipairs(market) do
  local id = market[k]
  local counter = get_order(id)
  if match_orders(order, counter) then
    log("found match")
    table.insert(matches, counter)
  end
end

--CHECK LIMIT ORDERS!
local limit = redis.call('ZRANGEBYSCORE', 'book:'..counter_type..':limit:'..order.symbol, 0, -1)
for k,v in ipairs(limit) do
  local id = limit[k]
  local counter = get_order(id)
  if match_orders(order, counter) then
    log("found match")
    table.insert(matches, counter)
  end
end

if table.getn(matches) == 0 then
  --day or until canceled?
  if order.tif == 'day' or order.tif == 'canceled' then
    return add_market_order(order)
  else
    return {"canceled", {{"reason", order.tif}}}
  end
else
    return {"test", {{"matched", orders_to_response(matches)}}}
end
