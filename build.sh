#!/bin/bash
sudo chown -R gbean:gbean /srv/market
rebar clean compile generate
ln -s /srv/lua/ rel/market/lua
sudo chown -R www-data:www-data /srv/market/rel/market
sudo supervisorctl restart erlmarket
