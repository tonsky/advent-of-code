#!/bin/bash -euo pipefail
cd "`dirname $0`/.."

clj -M -m nrepl.cmdline --interactive