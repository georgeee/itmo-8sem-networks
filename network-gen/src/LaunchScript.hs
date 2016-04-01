{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module LaunchScript where

import Env
import Text.RawString.QQ
import Text.Printf
import qualified Data.Char as C
import Output

newtype LaunchScript  =  LaunchScript Env

instance Show LaunchScript where
    show (LaunchScript e)  =  genLaunch e  

instance Output LaunchScript where
    defName _  =  "launch.sh"

---------------------- script start ---------------------
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
|] 
-------------------------- script end -------------------------

  where
    generate_devs :: String
    generate_devs = flip nodeCase (envNodes e) $ \node -> do
        Bridge{..} <- holdingBridges node e
        printf "   generate_dev %i %d %s\n" node bridgeId (C.toLower <$> show bridgeInetEnabled) :: String
            
    launches :: String
    launches = 
        let launch n = printf "launch %t %i %i &\n" n n n  
        in  envNodes e >>= launch
           
