#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mem=64
base_img=$DIR/base.img
network_script_up=$DIR/qemu-ifup.sh
network_script_down=$DIR/qemu-ifdown.sh

network_base="10.23"
network_mask="255.255.255.0"
network_br_prefix=qemu-br-

runConfigs=`pwd`/run-configs
mkdir $runConfigs
mkdir $runConfigs/net

function launch() {
  local type=$1
  local id=$2
  local i=$3
  echo "Launch $type $id"
  rm -Rf $runConfigs/$type$id
  mkdir -p $runConfigs/$type$id
  create_config $type $id $runConfigs/$type$id
  get_args $type $id $i | launch_qemu $type $id
}

function generate_script {
  local id=$1
  local inet=$2
  if [[ "$inet" == "" ]]; then
    inet=false
  fi
  local brId=$network_br_prefix$id
  local script_up=$runConfigs/net/bridge-$id-up.sh
  netscript_generate $id $inet > "$script_up"
  echo "$network_script_up "'$@' >> "$script_up"
  chmod +x "$script_up"
  local script_down=$runConfigs/net/bridge-$id-down.sh
  netscript_generate $id $inet > "$script_down"
  echo "$network_script_down "'$@' >> "$script_down"
  chmod +x "$script_down"
  echo "script=$script_up,downscript=$script_down"
}

function launch_qemu {
  local type=$1
  local id=$2

  xargs \
    sudo qemu-system-i386 -enable-kvm -m $mem -hda "$base_img" -snapshot -hdb fat:$runConfigs/$type$id -boot d
}

function netscript_generate {
  local id=$1
  echo "#!/bin/bash"
  echo "export BRIDGE=$network_br_prefix$id"
  echo "export NETWORK=$network_base.$id.0"
  echo "export GATEWAY=$network_base.$id.1"
  echo "export DHCPRANGE=$network_base.$id.2,$network_base.$id.254" 
  echo "export MASQUERADE=$2"
}

mac_counter=0

function generate_dev {
  local i=$1
  local id=$2 #bridge id
  local inet=$3 #true/false, enable inet (MASQUERADE)
  local mac=02-00-00-00-`printf "%02x" $i`-`printf "%02x" $mac_counter`
  echo -netdev tap,id=nic.$mac_counter,`generate_script $id $inet`
  echo -device e1000,netdev=nic.$mac_counter,mac=$mac
  mac_counter=$((mac_counter+1))
}

while test $# -gt 0
do
  case "$1" in
    -m) mem=$2
        shift
        ;;
    -b) base_img=$2
        shift
        ;;
    *) break
        ;;
  esac
  shift
done
