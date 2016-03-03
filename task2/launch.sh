#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

base_img=$1

mkdir run-configs

subnets=4

function launch() {
  type=$1
  id=$2
  _i=$3
  echo "Launch $type $id $_i"
  i="$type$id"
  rm -Rf run-configs/$i
  mkdir -p run-configs/$i
  create_config $type $id run-configs/$i
  mac_prefix=02-00-00-00-`printf "%02x" $_i`
  get_args $type $id $mac_prefix
  get_args $type $id $mac_prefix | xargs \
  	sudo qemu-system-i386 -enable-kvm -m 64 -hda "$base_img" -snapshot \
	 -hdb fat:run-configs/$i -boot d -net tap -net nic,macaddr=$mac_prefix-01
}

function create_config() {
  type=$1
  id=$2
  dir=$3
  cat "$DIR/init-stub.sh" | sed "s/%subnets%/$subnets/g" | sed "s/%id%/$id/g" | sed "s/%type%/$type/g" > "$dir/init.sh" 
  chmod +x "$dir/init.sh"
}

function get_args() {
  type=$1
  id=$2
  if [[ $id -eq 1 ]]; then
    p=$subnets$id
  else
    p=$((id-1))$id
  fi
  n=$id$((id%$subnets+1))
  mac_prefix=$3
  if [[ $type == 'm' ]]; then
#-netdev user,id=user.$p -device e1000,netdev=user.$p
     echo -netdev tap,id=user.$p -device e1000,netdev=user.$p,mac=$mac_prefix-02
     echo -netdev tap,id=user.$n -device e1000,netdev=user.$n,mac=$mac_prefix-03
     echo -netdev tap,id=user.$id -device e1000,netdev=user.$id,mac=$mac_prefix-04
  else
     echo -netdev tap,id=user.$id -device e1000,netdev=user.$id,mac=$mac_prefix-04
  fi
}

i=1
while [[ $i -le $subnets ]]; do 
  launch m $i $((2*i)) &
  launch s $i $((2*i+1)) &
  i=$((i+1))
done
