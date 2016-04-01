#!/bin/bash

###########################
### Common part
##########################

id=%id%
type=%type%
machine_id=%machine_id%

function ensure_dev_up {
  cnt=0
  ret=1
  while [[ $ret -ne 0 ]]; do
    cnt=$((cnt+1))
    ip link show $1 2>/dev/null 1>/dev/null
    ret=$?
    if [[ $cnt -gt 20 ]]; then
      echo "Dev $1 is dead"
      exit 3
    fi
    if [[ $ret -ne 0 ]]; then
      sleep 0.5
    fi
  done
  echo "Dev $1 is ready to up"
}

source "./ifaces.sh"

##########################################
## Task-specific part
##########################################

enable_inet=%enable_inet%
no_quagga=%no_quagga%

function get_iface {
  local i=$1
  local iface=${ifaces[$i]}
  echo $iface
}
function get_ipv6 {
  local i=$1
  local br=${bridges[$i]}
  local ind=${indexes[$i]}
  echo fc00:192:168:$br::$ind
}
function get_ipv4 {
  local i=$1
  local br=${bridges[$i]}
  local ind=${indexes[$i]}
  echo 192.168.$br.$ind
}

function ospf6d_conf {
  echo '!'
  echo 'password krispo'
  echo 'enable password krispo'
  echo 'log file /var/log/quagga/ospf6d.log'
  echo "service advanced-vty"
  echo '!'
  echo "router ospf6"
  echo "  router-id $machine_id.$machine_id.$machine_id.$machine_id"
  local i=0
  echo "  interface lo area 0.0.0.0"
  while [[ $i -lt ${#ifaces[*]} ]]; do
    echo "  interface `get_iface $i` area 0.0.0.0"
    i=$((i+1))
  done
  echo '!'
  local i=0
  while [[ $i -lt ${#ifaces[*]} ]]; do
    echo "interface `get_iface $i`"
    echo "  ipv6 ospf6 hello-interval 5"
    echo "  ipv6 ospf6 dead-interval 10"
    i=$((i+1))
  done
  echo "interface lo"
  echo "  ipv6 ospf6 hello-interval 5"
  echo "  ipv6 ospf6 dead-interval 10"
  echo '!'
}

function zebra_conf {
  echo 'password krispo'
  echo 'enable password krispo'
  echo 'log file /var/log/quagga/zebra.log'
  echo 'ip forwarding'
  local i=0
  while [[ $i -lt ${#ifaces[*]} ]]; do
    echo "interface `get_iface $i`"
    echo "  no ipv6 nd suppress-ra"
    echo "  ipv6 address `get_ipv6 $i`/64"
    echo "  ipv6 nd prefix `get_ipv6 $i`/64"
    echo "  ipv6 nd ra-interval 10"
    if [[ `opt s "$type"` != 0 ]]; then
      echo "interface lo"
      echo "  no ipv6 nd suppress-ra"
      echo "  ipv6 address fc00:192:168:215::1/64"
      echo "  ipv6 nd prefix fc00:192:168:215::1/64"
      echo "  ipv6 nd ra-interval 10"
    fi
    i=$((i+1))
  done
  echo "line vty"
}

if $enable_inet; then
  ensure_dev_up ens3
  ip link set ens3 up
  dhcpcd ens3
fi

function init_ifaces {
  local i=0
  while [[ $i -lt ${#ifaces[*]} ]]; do
    ensure_dev_up `get_iface $i`
    ifconfig `get_iface $i` `get_ipv4 $i`/24
    ifconfig `get_iface $i` inet6 add `get_ipv6 $i`/64
    i=$((i+1))
  done
}

init_ifaces

function opt {
  expr match "$2" ".*$1"
}

unzip timesync-bundle.zip
if [[ `opt m "$type"` != 0 ]]; then
  sysctl net.ipv4.ip_forward=1
  sysctl net.ipv6.conf.default.forwarding=1
  sysctl net.ipv6.conf.all.forwarding=1
  chown -Rf quagga:quagga /var/log/quagga
  ospf6d_conf | tee /etc/quagga/ospf6d.conf
  zebra_conf | tee /etc/quagga/zebra.conf
  if $no_quagga; then
    echo "Quagga not launched (do it manually)"
  else
    ospf6d -d -f /etc/quagga/ospf6d.conf
    zebra -d -f /etc/quagga/zebra.conf
  fi
fi
if [[ `opt l "$type"` != 0 ]]; then
  route add -net 192.168.0.0/16 gw 192.168.$id.1
  route -A inet6 add fc00:192:168::/48 gw fc00:192:168:$id::1
fi
if [[ `opt s "$type"` != 0 ]]; then
  ifconfig lo inet6 add fc00:192:168:215::1
  echo "launching timesync server"
  java -jar timesync/timesync.jar -s
fi
if [[ `opt c "$type"` != 0 ]]; then
  echo "launching timesync client"
  java -jar timesync/timesync.jar -c -a fc00:192:168:215::1
fi

echo -n "opt m: "
opt m "$type"
echo -n "opt l: "
opt l "$type"
echo -n "opt s: "
opt s "$type"
echo -n "opt c: "
opt c "$type"

echo "Quagga processes:"
ps -aux |grep quagga | grep -v grep

echo "Machine $type$id launched"

echo "export PS1='$type$id (inet: $enable_inet) >> '" > .bashrc

bash --rcfile .bashrc
