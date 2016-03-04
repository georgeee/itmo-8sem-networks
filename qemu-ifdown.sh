#!/bin/bash

iptables -F
iptables -t nat -F
ifconfig $BRIDGE down
brctl delbr $BRIDGE
kill `cat /var/run/qemu-dnsmasq-$BRIDGE.pid`
