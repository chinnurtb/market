from bottle import route, request, response, get, Bottle, \
                   jinja2_template as template, redirect, static_file, \
                   TEMPLATE_PATH, HTTPError

TEMPLATE_PATH.append("templates/")

static = Bottle()

from books import get as get_books
from quotes import get as get_quotes
from auth import auth
from db import get_inventory, get_user_orders, is_dark
import rpc

import constants

@static.route('/')
def index():
  books = prepare_books(get_books())
  symbols = constants.SYMBOLS
  c = {
      'books':books,
      'symbols':symbols
  }
  return template('index', c)

@static.route('/inventory/', method="POST")
@auth
def inventory():
  key = request.forms.get('key')
  return get_inventory(key)

@static.route('/orders/', method="POST")
@auth
def orders():
  key = request.forms.get('key')
  return get_user_orders(key)

@static.route('/buy/', method="POST")
@auth
def buy():
  key = request.forms.get('key')
  data = validate_order(request.forms)
  if data is not False:
    inventory = get_inventory(key)
    symbol, quantity, qconst, limit, tif = data
    price = current_price(symbol)
    if not is_dark(key):
      if quantity * price > int(inventory['cash']):
        raise HTTPError(400, "NSF")
    return rpc.buy(key, symbol, limit, quantity, qconst, tif)
  else:
    raise HTTPError(400, "BAD ARGUMENTS")


@static.route('/sell/', method="POST")
@auth
def sell():
  key = request.forms.get('key')
  data = validate_order(request.forms)
  if data is not False:
    inventory = get_inventory(key)
    symbol, quantity, qconst, limit, tif = data
    if not is_dark(key):
      if quantity > int(inventory[symbol]):
        raise HTTPError(400, "NOT ENOUGH %s" % symbol)
    return rpc.sell(key, symbol, limit, quantity, qconst, tif)
  else:
    raise HTTPError(400, "BAD ARGUMENTS")

  

# Static Routes
@static.get('/<filename:re:.*\.js>')
def javascripts(filename):
    return static_file(filename, root='static/js')

@static.get('/<filename:re:.*\.css>')
def stylesheets(filename):
    return static_file(filename, root='static/css')

@static.get('/<filename:re:.*\.(jpg|png|gif|ico)>')
def images(filename):
    return static_file(filename, root='static/img')

def prepare_books(books):
  ret = {}
  for symbol in books:
    bids = books[symbol]['market']['bid']
    bids.extend(books[symbol]['limit']['bid'])
    def sort(o):
      if o['limit'] == 'none': return 0
      return int(o['limit'])
    bids.sort(key=sort, reverse=True)
    ret[symbol] = get_quotes(symbol)
    ret[symbol]['name'] = symbol
    ret[symbol]['bids'] = bids
    asks = books[symbol]['market']['ask']
    asks.extend(books[symbol]['limit']['ask'])
    asks.sort(key=sort)
    ret[symbol]['asks'] = asks
  return ret

def validate_order(params):
  symbol = params.get("symbol", None)
  quantity = params.get("quantity", None)
  qconst = params.get("quantity_constraint", "none")
  limit = params.get("limit", "none")
  tif = params.get("time_in_force", "cancelled")
  if None in [ symbol, quantity ]:
    return False

  try:
    if not limit == "none":
      limit = int(limit)
  except:
    return False

  try:
    quantity = int(quantity)
  except:
    return False

  if qconst in ["all" or "none"] is False:
    return False

  if tif not in ["day", "cancelled", "fill", "immediate"]:
    return False

  return symbol, quantity, qconst, limit, tif

def current_price(symbol):
  stats = get_quotes(symbol)
  price = stats['ask_low']
  if price == 0:
    price = stats['price']
  return price

