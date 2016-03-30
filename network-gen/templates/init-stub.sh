#!/bin/bash

###########################
### Common part
##########################

id=%id%
type=%type%

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

##########################################
## Task-specific part
##########################################

subnets=%subnets%
enable_inet=%enable_inet%
no_quagga=%no_quagga%


if $enable_inet; then
  ensure_dev_up ens3
  ip link set ens3 up
  dhcpcd ens3
fi

if [[ $type == 'm' ]]; then
  echo "1" > /proc/sys/net/ipv4/ip_forward
  echo -n "/proc/sys/net/ipv4/ip_forward: "
  cat /proc/sys/net/ipv4/ip_forward
  chown -Rf quagga:quagga /var/log/quagga
  ripd_conf | tee /etc/quagga/ripd.conf
  zebra_conf | tee /etc/quagga/zebra.conf
  if $no_quagga; then
    echo "Quagga not launched (do it manually)"
  else
    ripd -d -f /etc/quagga/ripd.conf
    zebra -d -f /etc/quagga/zebra.conf
  fi
else

  route add -net 192.168.0.0/16 gw 192.168.$id.1
fi

ifconfig
route -n

echo "Machine $type$id launched"

echo "export PS1='$type$id (inet: $enable_inet) >> '" > .bashrc

bash --rcfile .bashrc
