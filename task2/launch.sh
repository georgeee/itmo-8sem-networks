#!/bin/bash

### Launch.sh
### Launches virtual machine cluster on several qemu instances

### This script is to be adapted for concrete application
### Basic idea: each machine in cluster has type and id


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $DIR/launch-base.sh

subnets=4
launch_slave=true
launch_master=true
ei=false
no_quagga=false

while test $# -gt 0
do
  case "$1" in
    -m) mem=$2
        shift
        ;;
    -b) base_img=$2
        shift
        ;;
    -no-m) launch_master=false
        ;;
    -no-s) launch_slave=false
        ;;
    -ei) ei=true
        ;;
    -nq) no_quagga=true
        ;;
    -s) subnets=$2
        shift
        ;;
  esac
  shift
done

function create_config() {
  local type=$1
  local id=$2
  local dir=$3
  cat "$DIR/init-stub.sh" \
    | sed "s/%enable_inet%/$ei/g" \
    | sed "s/%id%/$id/g" \
    | sed "s/%type%/$type/g" \
    | sed "s/%subnets%/$subnets/g" \
    | sed "s/%no_quagga%/$no_quagga/g" \
    > "$dir/init.sh"
  chmod +x "$dir/init.sh"
}

function get_args() {
  local type=$1
  local id=$2
  local i=$3
  if [[ $id -eq 1 ]]; then
    local p=$subnets$id
  else
    local p=$((id-1))$id
  fi
  local n=$id$((id%$subnets+1))

  echo "$i ($type$id) p=$p n=$n" >&2

  generate_dev $i $id $ei
  if [[ $type == 'm' ]]; then
     generate_dev $i $p
     generate_dev $i $n
     generate_dev $i $id $ei
  else
     generate_dev $i $id $ei
  fi
}

id=1
i=1
while [[ $id -le $subnets ]]; do 
  if $launch_master; then
    launch m $id $((2*i-1)) &
  fi
  if $launch_slave; then
    launch s $id $((2*i)) &
  fi
  i=$((i+1))
  id=$((id+1))
done
