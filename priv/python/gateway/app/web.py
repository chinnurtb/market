from bottle import route, request, response, get, Bottle, \
                   jinja2_template as template, redirect, static_file, \
                   TEMPLATE_PATH

TEMPLATE_PATH.append("templates/")

static = Bottle()

from books import get as get_books
from quotes import get as get_quotes
from auth import auth
from db import get_inventory

import constants

@static.route('/')
def index():
  books = prepare_books(get_books())
  symbols = constants.SYMBOLS
  c = {
      'books':books,
      'symbols':symbols
  }
  return template('index.html', c)

@static.route('/inventory/', method="POST")
@auth
def inventory():
  key = request.forms.get('key')
  return get_inventory(key)

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
