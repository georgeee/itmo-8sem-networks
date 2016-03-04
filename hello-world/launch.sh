#!/bin/bash

### Launch.sh
### Launches virtual machine cluster on several qemu instances

### This script is to be adapted for concrete application
### Basic idea: each machine in cluster has type and id (subnetwork id)


APP_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $APP_DIR/../launch-base.sh

### Arguments

## Here you can customize arguments for your setup
## Note that basic options like -m and -b would be parsed in launch-base.sh, so you don't have to worry about them

ei=false #enable_internet
msg="Hello, world"
subnets=2


while test $# -gt 0
do
  case "$1" in
    -s) subnets=$2
        shift
        ;;
    -ei) ei=true
        ;;
    -msg) msg="$2"
          shift
          ;;
    *) break
        ;;
  esac
  shift
done

### VM config generator
## In this function you need to generate files for VM's environment
## On VM start init.sh will be launched (if exists)

# create_config <type> <id> <dir>

function create_config() {
  local type=$1
  local id=$2
  local dir=$3
  cat "$APP_DIR/init-stub.sh" \
    | sed "s/%id%/$id/g" \
    | sed "s/%type%/$type/g" \
    | sed "s/%enable_inet%/$ei/g" \
    | sed "s/%subnets%/$subnets/g" \
    | sed "s/%msg%/$msg/g" \
    > "$dir/init.sh"
  chmod +x "$dir/init.sh"
}

### QEMU additional arguments generator
## Here you can generate additional arguments for QEMU, simply echoing them to stdout
## Use generate_dev subroutine to setup necessary devices

# get_args <type> <id> <machine_id>

# generate_dev <machine_id> <id> <enable_inet>
#
# if enable_inet=true, internet would be appropriately set up
#   otherwise (enable_inet=false) internet would be off for VM
# id is an id of bridge, should be in range [1..254]
# if two devices are connected to same bridge, they will share one network

function get_args() {
  local type=$1
  local id=$2
  local i=$3

  # In this example 100 is a network for all devices

  generate_dev $i $id $ei
  if [[ $type == 'm' ]]; then
     generate_dev $i 100 $ei
  else
     generate_dev $i 100 $ei
  fi
}

### Main section
## This section launches all VMs

## Use launch subroutine to launch VMs
## Please note, that machine_id values should be unique
## As well as pairs {type, id}

# launch <type> <id> <machine_id>

id=1
i=1
while [[ $id -le $subnets ]]; do 
  launch tyler $id $((2*i-1)) &
  launch narrator $id $((2*i)) &
  i=$((i+1))
  id=$((id+1))
done
