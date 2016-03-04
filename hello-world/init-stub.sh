#!/bin/bash

###########################
### Common part
##########################

id=%id%
type=%type%

if [[ $id -eq 1 ]]; then
  p=$subnets$id
else
  p=$((id-1))$id
fi
n=$id$((id%$subnets+1))

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

enable_inet=%enable_inet%
msg='%msg%'

if $enable_inet; then
  ensure_dev_up ens3
  ip link set ens3 up
  dhcpcd ens3
fi

if [[ $type == 'tyler' ]]; then
  ensure_dev_up ens4
  ifconfig ens4 192.168.$id.1
elif [[ $type == 'narrator' ]]; then
  ensure_dev_up ens4
  ifconfig ens4 192.168.$id.2
fi
route add -net 192.168.0.0/16 dev ens4

echo "Machine $type$id launched"

echo "Internet enabled: $enable_inet"
ping -c 3 8.8.8.8

echo "Message: $msg"

echo "export PS1='$type$id (inet: $enable_inet) >> '" > .bashrc

bash --rcfile .bashrc
