import redis
from pygooglechart import *
r = redis.StrictRedis()

symbols = ["cocaine", "pot", "speed", "acid", "shrooms", "oxy", "valium", "pcp", "hash", "percocet", "mdma"]

metrics = {}

for symbol in symbols:
  print symbol

  prices = r.zrange("historical:price:" + symbol, 0, -1)
  prices = [int(p) for p in prices]
  volumes = r.zrange("historical:volume:" + symbol, 0, -1)
  volumes = [int(v) for v in volumes]

  chart = SimpleLineChart(300, 200)
  chart.add_data(prices)
  chart.download(symbol + "-prices.png")

  chart = SimpleLineChart(300, 200)
  chart.add_data(volumes)
  chart.download(symbol + "-volume.png")

  metrics[symbol] = {}
  metrics[symbol]["price"] = prices
  metrics[symbol]["volume"] = volumes


