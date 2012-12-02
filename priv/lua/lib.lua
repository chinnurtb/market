local log = function(msg)
  redis.log(redis.LOG_DEBUG, msg)
end
local get_quote = function(symbol)
  return tonumber(redis.call('HGET', symbol, 'tick')) or 1
end
local get_book = function(side, type, symbol)
  local market = redis.call('SORT',
    'book:'..side..':market:'..type..symbol, 'BY', 'NOSORT',
    'GET', '*->id', 'GET', '*->limit', 'GET', '*->quantity',
    'GET', '*->quantity_constraint','GET', '*->tif',
    'GET', '*->ts', 'GET', '*->user', 'ASC')
  local ret = {}
  for i,order in ipairs(market) do
    table.insert(ret, make_record


local get_order = function(id)
  local o = redis.call('HMGET','order:'..id,
    'limit', 'quantity', 'quantity_constraint',
    'tif', 'ts', 'user', 'symbol', 'type')
  return {
    id=id,
    limit=o[1],
    quantity=o[2],
    quantity_constraint=o[3],
    tif=o[4],
    ts=o[5],
    user=o[6],
    symbol=o[7],
    type=o[8]
  }
end
local delete_order = function(order)
  if order.id then
    redis.call('DEL', 'order:'..order.id)
    redis.call('SREM', 'user:'..order.user..':orders', order.id)
    redis.call('ZREM', 'book:'..order.type..':market:'..order.symbol, order.id)
  end
end
local get_order_id = function(order)
  local id = redis.call("INCR", "order:ids")
  return order.ts .. "-" .. id
end
local make_order = function(user, symbol, type, limit, quantity, tif, qconst, ts)
  return {
    type=type,
    limit=limit,
    quantity=quantity,
    quantity_constraint=qconst,
    tif=tif,
    ts=ts,
    user=user,
    symbol=symbol
  }
end
local order_to_response = function(order)
  return {
    {'type', order.type},
    {'limit', order.limit},
    {'quantity', order.quantity},
    {'quantity_constraint', order.quantity_constraint},
    {'tif', order.tif},
    {'ts', order.ts},
    {'user', order.user},
    {'symbol', order.symbol},
    {'id', order.id}
  }
end
local orders_to_response = function(orders)
  local ret = {}
  for k,v in ipairs(orders) do
    table.insert(ret, order_to_response(v))
  end
  return ret
end
local get_user = function(id)
  local u = redis.call('HMGET', 'user:'..id,
    'cash', 'cocaine', 'acid',
    'speed', 'pot', 'oxycodone',
    'valium', 'percocet', 'mdma',
    'pcp')
  return {
    id=id,
    cash=u[1],
    cocaine=u[2],
    acid=u[3],
    speed=u[4],
    pot=u[5],
    oxycodone=u[6],
    valium=u[7],
    percocet=u[8],
    mdma=u[9],
    pcp=u[10]
  }
end
local is_market_order = function(o)
  return o.limit == 0 or true
end
local is_limit_order = function(o)
  return o.limit > 0 or false
end
local meets_quantity_constraint = function(buy, sell)
  if (sell.quantity_constraint == 'all' and 
    sell.quantity > buy.quantity) then
    log("sell has all quantity restraint higher than buy quantity")
    return false
  end
  if (buy.quantity_constraint == 'all' and
    sell.quantity < buy.quantity) then
    log("buy has all quantity restraint higher than sell quantity")
    if buy.tif == 'fill' or buy.tif == 'immediate' then
      log("buy is fill or immediate")
      return false
    end
  end
  return true
end
--DO NOT LET USERS PLACE MARKET OR LIMIT ORDERS
--OF THE SAME TYPE AT THE SAME TIME ( BUY/SELL )
local can_place_order = function(order)
  local existing = redis.call('SMEMBERS', 'user:'..order.user..':orders')
  for k,v in ipairs(existing) do
    log("got existing order "..v)
    local e = get_order(v)
    if not e.limit then
      log("stale order "..v)
      redis.call('SREM', 'user:'..order.user..':orders', v)
    else 
      log("good order for "..e.symbol)
      if e.symbol == order.symbol then
        log("same symbol")
        if e.type == order.type then
         log("same order type")
         return false
        end
      end
    end
  end
  log("good to go")
  return true
end
local match_orders = function(buy, sell)
  log("match_order")
  if buy.user == sell.user then
    log("same user")
    return false
  end
  -- NOT SURE ABOUT THIS
  if is_limit_order(buy) then
    log("buy limit order")
    if is_limit_order(sell) then
      log("sell limit order")
      if buy.limit < sell.limit then
        log("buy limit less than sell limit")
        return false
      end
    end
  end
  if not meets_quantity_constraint(buy, sell) then
    log("fails quantity constraint")
    return false
  end
  return true
end
local add_market_order = function(order)
  log("add_market_order")
  order.id = get_order_id(order)
  redis.call('HMSET', 'order:'..order.id, 'limit', order.limit, 'quantity', order.quantity, 'quantity_constraint', order.quantity_constraint, 'tif', order.tif, 'ts', order.ts, 'user', order.user, 'symbol', order.symbol, 'type', order.type)
  redis.call('SADD', 'user:'..order.user..':orders', order.id)
  redis.call('ZADD', 'book:'..order.type..':market:'..order.symbol, order.quantity, order.id)
  local ot = order_to_response(order)
  return {"accepted", ot}
end
local attempt_to_fill = function(buy, sell, ts)
  log("buy order user is "..buy.user)
  log("sell order user is "..sell.user)
  local buy_u = get_user(buy.user)
  local sell_u = get_user(sell.user)

  local unit_price = tonumber(sell.limit) or tonumber(buy.limit) or 0
  if unit_price == 0 then
    --pull quote
    unit_price = get_quote(buy.symbol)
  end
  local quantity = 0
  if buy.quantity < sell.quantity then
    quantity = tonumber(buy.quantity)
  else
    quantity = tonumber(sell.quantity)
  end
  local tx_cost = quantity * unit_price
  local cash = tonumber(buy_u.cash) or 0
  local on_hand = tonumber(sell_u[sell.symbol]) or 0
  log(buy_u.id.." buying "..quantity.." of "..buy.symbol.." for "..unit_price.." per total "..tx_cost.." from "..sell_u.id)
  log("seller has "..(sell_u[sell.symbol] or 'none'))
  log("buyer has "..(buy_u.cash or 'none'))
  if cash > tx_cost then
    if on_hand > quantity then
      log("doing tx")
      local tx_id = redis.call('INCR', 'tx:ids')
      redis.call('HINCRBY', 'user:'..buy_u.id, buy.symbol, quantity)
      redis.call('HINCRBY', 'user:'..buy_u.id, 'cash', tx_cost * (-1))
      redis.call('HINCRBY', 'user:'..sell_u.id, buy.symbol, quantity*(-1))
      redis.call('HINCRBY', 'user:'..sell_u.id, 'cash', tx_cost)
      redis.call('HMSET', 'tx:'..tx_id, 'buyer', buy_u.id, 'seller', sell_u.id,
                'quantity', quantity, 'price', unit_price, 'total', tx_cost,
                'symbol', buy.symbol, 'ts', ts)
      redis.call('SADD', 'user:'..buy_u.id..':transactions', tx_id)
      redis.call('SADD', 'user:'..sell_u.id..':transactions', tx_id)
      redis.call('LPUSH', buy.symbol..':transactions', tx_id)
      redis.call('LTRIM', buy.symbol..':transactions', 0, 1000)
      redis.call('ZADD', buy.symbol..':ticks', unit_price, tx_id)
      redis.call('HSET', buy.symbol, 'tick', unit_price)
      log("transaction "..tx_id.." complete")

      delete_order(sell)
      delete_order(buy)
      return tx_id
    else
      log("seller unable to fill order "..(buy.id or 0).." : "..(sell.id or 0))
      delete_order(sell)
      return -2
    end
  else
    log("buyer unable to fill order "..(buy.id or 0).." : "..(sell.id or 0))
    delete_order(buy)
    return -1
  end
end
