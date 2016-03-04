#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

base_img=$DIR/../base.img
network_script_up=$DIR/../qemu-ifup.sh
network_script_down=$DIR/../qemu-ifdown.sh

mkdir run-configs
mkdir run-configs/net

subnets=4
mem=64

launch_slave=true
launch_master=true

enable_inet=false
no_quagga=false

network_base="10.27"
network_mask="255.255.255.0"
network_br_prefix=qemu-br-

while test $# -gt 0
do
  case "$1" in
    -m) mem=$2
        shift
        ;;
    -s) subnets=$2
        shift
        ;;
    -b) base_img=$2
        shift
        ;;
    -no-m) launch_master=false
        ;;
    -no-s) launch_slave=false
        ;;
    -nq) no_quagga=true
        ;;
    -ei) enable_inet=true
        ;;
  esac
  shift
done

echo "enable_inet: $enable_inet"

function launch() {
  type=$1
  id=$2
  _i=$3
  echo "Launch $type $id $_i"
  i="$type$id"
  rm -Rf run-configs/$type$id
  mkdir -p run-configs/$type$id
  create_config $type $id run-configs/$type$id
  get_args $type $id $_i
  get_args $type $id $_i | launch_qemu $type $id $_i
}

function generate_script {
  i=$1
  inet=$2
  if [[ "$inet" == "" ]]; then
    inet=false
  fi
  brId=$network_br_prefix$i
  script_up=run-configs/net/bridge-$i-up.sh
  netscript_generate $i $inet > "$script_up"
  echo "$network_script_up "'$@' >> "$script_up"
  chmod +x "$script_up"
  script_down=run-configs/net/bridge-$i-down.sh
  netscript_generate $i $inet > "$script_down"
  echo "$network_script_down "'$@' >> "$script_down"
  chmod +x "$script_down"
  echo "script=$script_up,downscript=$script_down"
}

function launch_qemu {
  # # Set to the name of your bridge
  # BRIDGE=br0
  # 
  # # Network information
  # NETWORK=192.168.53.0
  # GATEWAY=192.168.53.1
  # DHCPRANGE=192.168.53.2,192.168.53.254
  type=$1
  id=$2
  _i=$3

  xargs \
    sudo qemu-system-i386 -enable-kvm -m $mem -hda "$base_img" -snapshot -hdb fat:run-configs/$type$id -boot d
}

function netscript_generate {
  i=$1
  echo "#!/bin/bash"
  echo "export BRIDGE=$network_br_prefix$i"
  echo "export NETWORK=$network_base.$i.0"
  echo "export GATEWAY=$network_base.$i.1"
  echo "export DHCPRANGE=$network_base.$i.2,$network_base.$i.254" 
  echo "export MASQUERADE=$2"
}

function create_config() {
  type=$1
  id=$2
  dir=$3
  cat "$DIR/init-stub.sh" | sed "s/%enable_inet%/$enable_inet/g" \
                          | sed "s/%subnets%/$subnets/g" \
                          | sed "s/%id%/$id/g" \
                          | sed "s/%no_quagga%/$no_quagga/g" \
                          | sed "s/%type%/$type/g" > "$dir/init.sh" 
  chmod +x "$dir/init.sh"
}

function get_args() {
  type=$1
  id=$2
  _i=$3
  mac_prefix=02-00-00-00-`printf "%02x" $_i`
  if [[ $id -eq 1 ]]; then
    p=$subnets$id
  else
    p=$((id-1))$id
  fi
  n=$id$((id%$subnets+1))
  echo -netdev tap,id=nic.$id,`generate_script $id $enable_inet` -device e1000,netdev=nic.$id,mac=$mac_prefix-01
  if [[ $type == 'm' ]]; then
     echo -netdev tap,id=user.$p,`generate_script $p` -device e1000,netdev=user.$p,mac=$mac_prefix-02
     echo -netdev tap,id=user.$n,`generate_script $n` -device e1000,netdev=user.$n,mac=$mac_prefix-03
     echo -netdev tap,id=user._$id,`generate_script $((100+$id))` -device e1000,netdev=user._$id,mac=$mac_prefix-04
  else
     echo -netdev tap,id=user._$id,`generate_script $((100+$id))` -device e1000,netdev=user._$id,mac=$mac_prefix-04
  fi
}

i=1
while [[ $i -le $subnets ]]; do 
  if $launch_master; then
    launch m $i $((2*i-1)) &
  fi
  if $launch_slave; then
    launch s $i $((2*i)) &
  fi
  i=$((i+1))
done
