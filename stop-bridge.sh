#!/bin/bash

restore=false

if [[ "$1" == '-r' ]]; then
  restore=true
  shift
fi

bridgeId=$1

mkdir -p stopped-bridges
brfile=stopped-bridges/br$bridgeId

if $restore; then
  cat $brfile | while read i; do
    sudo brctl addif qemu-br-$bridgeId $i
  done
  rm $brfile
else
  echo -n '' > $brfile
  sudo brctl show qemu-br-$bridgeId| sed -r 's/^\s*(\S+\s+)*(\S+)/\2/'| tail -n +2| while read i; do
    sudo brctl delif qemu-br-$bridgeId $i
    echo $i >> $brfile
  done
fi
