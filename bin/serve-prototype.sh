#!/usr/bin/env bash

set -e

port="${1:-3000}"

caddy file-server --browse --root prototype --listen :"$port"
