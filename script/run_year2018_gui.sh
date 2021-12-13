#!/bin/bash -euo pipefail
cd "`dirname $0`/.."

clj -J-XstartOnFirstThread -A:humbleui -M -m advent-of-code.year2018.gui