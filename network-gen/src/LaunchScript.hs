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
    defName _  =  "launch-impl.sh"

---------------------- script start ---------------------
genLaunch :: Env -> String
genLaunch e  =  [r|#!/bin/bash

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

|] ++ launches
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

