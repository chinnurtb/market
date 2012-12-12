from bottle import route, request, response, get, Bottle, \
                   jinja2_template as template, redirect, static_file, \
                   TEMPLATE_PATH

TEMPLATE_PATH.append("templates/")

static = Bottle()

from books import get as get_books
from quotes import get as get_quotes

@static.route('/')
def index():
  return template('index.html', prepare_books(get_books()))

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
    bids.append(books[symbol]['limit']['bid'])
    ret[symbol] = get_quotes(symbol)
    ret[symbol]['bids'] = bids
    asks = books[symbol]['market']['ask']
    asks.append(books[symbol]['limit']['ask'])
    ret[symbol]['asks'] = asks
  return ret
