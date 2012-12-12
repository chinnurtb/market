import constants

def chunks(l, n):
  return [l[i:i+n] for i in range(0, len(l), n)]

def zip_order(order):
  return dict(zip(constants.ORDER_PROPS, order))

def zip_orders(orders):
  orders = chunks(orders, len(constants.ORDER_PROPS))
  return [dict(zip(constants.ORDER_PROPS, o)) for o in orders]

def zip_txn(txn):
  return dict(zip(constants.TXN_PROPS, txn))

def zip_symbol_data(data):
  return dict(zip(constants.SYMBOL_DATA_PROPS, data))
