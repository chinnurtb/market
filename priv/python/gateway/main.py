import gevent
from gevent import pywsgi
from app.web import static
from app.events import market_events
from app.books import run as books_run
from app.quotes import run as quotes_run
from bottle import run


def run_http():
  run(static, debug=False, port=8080, host='0.0.0.0')

if __name__ == "__main__":
  http = gevent.spawn(run_http)
  events = gevent.spawn(market_events)
  books = gevent.spawn(books_run)
  quotes = gevent.spawn(quotes_run)
  gevent.joinall([http, events, books, quotes])

