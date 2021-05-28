#!/bin/bash

name="${1}"

if [ -z "${name}" ]; then
    echo "Missing name (e.g. 04-lazy-optim)"
    exit
fi

echo "Benchmarking ${name}..."
go test ./parser \
    -bench=. \
    -run=TestXXX \
    -benchtime=1000x \
    -outputdir=profiles \
    -cpuprofile "${name}-cpu.out" \
    -memprofile "${name}-mem.out"
