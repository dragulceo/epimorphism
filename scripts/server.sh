#!/usr/bin/env bash

cd save_server
php -S localhost:9000 &

cd ../html
http-server -p 8000
