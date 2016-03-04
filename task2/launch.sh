#!/bin/bash

### Launch.sh
### Launches virtual machine cluster on several qemu instances

### This script is to be adapted for concrete application
### Basic idea: each machine in cluster has type and id (subnetwork id)


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source $DIR/../launch-base.sh

### Arguments

## Here you can customize arguments for your setup
## Note that basic options like -m and -b would be parsed in launch-base.sh, so you don't have to worry about them

subnets=4
launch_slave=true
launch_master=true
ei=false #enable_internet
no_quagga=false

while test $# -gt 0
do
  case "$1" in
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
  cat "$DIR/init-stub.sh" \
    | sed "s/%enable_inet%/$ei/g" \
    | sed "s/%id%/$id/g" \
    | sed "s/%type%/$type/g" \
    | sed "s/%subnets%/$subnets/g" \
    | sed "s/%no_quagga%/$no_quagga/g" \
    > "$dir/init.sh"
  chmod +x "$dir/init.sh"
}

### QEMU additional arguments generator
## Here you can generate additional arguments for QEMU, simply echoing them to stdout
## Use generate_dev subroutine to setup necessary devices

# get_args <type> <id> <machine_id>

# generate_dev <machine_id> <id> <enable_inet>
# (if enable_inet=true, internet would be appropriately set up
#  otherwise (enable_inet=false) internet would be off for VM

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

### Main section
## This section launches all VMs

## Use launch subroutine to launch VMs
## Please note, that machine_id values should be unique
## As well as pairs {type, id}

# launch <type> <id> <machine_id>

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
