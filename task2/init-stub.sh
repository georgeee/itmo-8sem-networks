#!/bin/bash

subnets=%subnets%
id=%id%
type=%type%
if [[ $id -eq 1 ]]; then
  p=$subnets$id
else
  p=$((id-1))$id
fi
n=$id$((id%$subnets+1))

function ripd_conf {
  echo '!'
  echo 'password krispo'
  echo 'router rip'
  echo 'route 192.168.0.0/16'
  echo 'network 192.168.0.0/16'
  echo 'network ens4'
  echo 'network ens5'
  echo "neighbor 192.168.$p.2"
  echo "neighbor 192.168.$n.1"
  echo 'log file /var/log/quagga/ripd.log'
  echo '!'
}

function zebra_conf {
  echo 'password krispo'
  echo 'enable password krispo'
  echo 'log file /var/log/quagga/zebra.log'
  echo 'interface ens4'
  echo '  link-detect'
  echo 'interface ens5'
  echo '  link-detect'
}

if [[ $type == 'm' ]]; then
  ifconfig ens4 192.168.$p.1
  ifconfig ens5 192.168.$n.2
  ifconfig ens6 192.168.$id.1
  echo "1" > /proc/sys/net/ipv4/ip_forward
  echo -n "/proc/sys/net/ipv4/ip_forward: "
  cat /proc/sys/net/ipv4/ip_forward
  chown -Rf quagga:quagga /var/log/quagga
  ripd_conf | tee /etc/quagga/ripd.conf
  zebra_conf | tee /etc/quagga/zebra.conf
  ripd -d -f /etc/quagga/ripd.conf
  zebra -d -f /etc/quagga/zebra.conf
else
  ifconfig ens4 192.168.$id.2
fi
