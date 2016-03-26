{-# LANGUAGE QuasiQuotes #-}

module LaunchScript where

import Env
import Text.RawString.QQ
import Text.Printf
import qualified Data.Char as C
import Scripts

newtype LaunchScript  =  LaunchScript Env

instance Show LaunchScript where
    show (LaunchScript e)  =  genLaunch e  

instance Script LaunchScript where
    defName  =  const "launch.sh"

genLaunch :: Env -> String
genLaunch e  =  [r|  
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

launch_slave=true
launch_master=true
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
  cat "$APP_DIR/init-stub.sh" \
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
#
# if enable_inet=true, internet would be appropriately set up
#   otherwise (enable_inet=false) internet would be off for VM
# id is an id of bridge, should be in range [1..254]
# if two devices are connected to same bridge, they will share one network

function get_args() {
  local type=$1
  local id=$2
  local i=$3
  local name=$type$id
|] ++ generate_devs ++ [r|  

}

### Main section
## This section launches all VMs

## Use launch subroutine to launch VMs
## Please note, that machine_id values should be unique
## As well as pairs {type, id}

# launch <type> <id> <machine_id>

|] ++ launches ++ [r|
done
|] -- template end

  where
    generate_devs :: String
    generate_devs = 
        let bridges = envBridges e
            print_dev n (Bridge ie bid _) = printf "   generate_dev '%i' '%d' %s\n" n bid (C.toLower <$> show ie) :: String
            devs n  = (filter (elem n . bridgeNodes) bridges) >>= print_dev n :: String
            node_dev n = printf "if [[ $name == '%s' ]]; then\n%sfi\n" n (devs n)
        in  envNodes e >>= node_dev

    launches :: String
    launches = 
        let launch n = printf "launch %t %i %s &\n" n n n  
        in  envNodes e >>= launch
           
