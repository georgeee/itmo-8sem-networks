#!/bin/bash


subnets=%subnets%
id=%id%
enable_inet=%enable_inet%
type=%type%
no_quagga=%no_quagga%

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

if $enable_inet; then
  ensure_dev_up ens3
  ip link set ens3 up
  dhcpcd ens3
fi

if [[ $type == 'm' ]]; then
  ensure_dev_up ens4
  ensure_dev_up ens5
  ensure_dev_up ens6
  ifconfig ens4 192.168.$p.1
  ifconfig ens5 192.168.$n.2
  ifconfig ens6 192.168.$id.1
  echo "1" > /proc/sys/net/ipv4/ip_forward
  echo -n "/proc/sys/net/ipv4/ip_forward: "
  cat /proc/sys/net/ipv4/ip_forward
  chown -Rf quagga:quagga /var/log/quagga
  ripd_conf | tee /etc/quagga/ripd.conf
  zebra_conf | tee /etc/quagga/zebra.conf
  if [[ ! $no_quagga ]]; then
    ripd -d -f /etc/quagga/ripd.conf
    zebra -d -f /etc/quagga/zebra.conf
  fi
else
  ensure_dev_up ens4
  ifconfig ens4 192.168.$id.2
  route add -net 192.168.0.0/16 gw 192.168.$id.1
fi

ifconfig
route -n

echo "Machine $type$id launched"

echo "export PS1='$type$id (inet: $enable_inet) >> '" > .bashrc

bash --rcfile .bashrc
