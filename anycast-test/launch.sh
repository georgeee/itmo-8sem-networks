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

ei=true #enable_internet
no_quagga=false

while test $# -gt 0
do
  case "$1" in
    -ei) ei=true
        ;;
    -nq) no_quagga=true
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
  local i=$4
  cat "$APP_DIR/init-stub.sh" \
    | sed "s/%enable_inet%/$ei/g" \
    | sed "s/%id%/$id/g" \
    | sed "s/%machine_id%/$i/g" \
    | sed "s/%type%/$type/g" \
    | sed "s/%no_quagga%/$no_quagga/g" \
    > "$dir/init.sh"
  cp "ifaces.sh" "$dir"
  cp -Rf bundle/* "$dir"
  chmod +x "$dir/init.sh"
  chmod +x "$dir/ifaces.sh"
}

source $APP_DIR/launch-impl.sh
