#!/bin/bash
cd "$(dirname "$0")"

cd scotch-db && sos 2>&1 &
cd scotch-workers && sos 2>&1 &
cd scotch-web-server && sos 2>&1
