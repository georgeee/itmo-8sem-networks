#!/bin/bash

iptables -w -F 1>/dev/null 2>/dev/null
iptables -w -t nat -F 1>/dev/null 2>/dev/null
ifconfig $BRIDGE down 1>/dev/null 2>/dev/null
brctl delbr $BRIDGE 1>/dev/null 2>/dev/null
kill `cat /var/run/qemu-dnsmasq-$BRIDGE.pid` 1>/dev/null 2>/dev/null

exit 0
