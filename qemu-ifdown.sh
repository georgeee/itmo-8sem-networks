#!/bin/bash

iptables -w -F
iptables -w -t nat -F
ifconfig $BRIDGE down
brctl delbr $BRIDGE
kill `cat /var/run/qemu-dnsmasq-$BRIDGE.pid`
