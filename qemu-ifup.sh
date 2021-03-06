#!/bin/sh
#
# Copyright IBM, Corp. 2010  
#
# Authors:
#  Anthony Liguori <aliguori@us.ibm.com>
#
# This work is licensed under the terms of the GNU GPL, version 2.  See
# the COPYING file in the top-level directory.

#variables to be set via environment

# # Set to the name of your bridge
# BRIDGE=br0
# 
# # Network information
# NETWORK=192.168.53.0
# NETMASK=255.255.255.0
# GATEWAY=192.168.53.1
# DHCPRANGE=192.168.53.2,192.168.53.254

echo "$0: arg1=$1 BRIDGE=$BRIDGE NETWORK=$NETWORK GATEWAY=$GATEWAY DHCPRANGE=$DHCPRANGE MASQUERADE=$MASQUERADE"

# Optionally parameters to enable PXE support
TFTPROOT=
BOOTP=

do_brctl() {
    brctl "$@"
}

do_ifconfig() {
    ifconfig "$@"
}

do_dd() {
    dd "$@"
}

do_iptables() {
    iptables -w "$@"
}

do_dnsmasq() {
    dnsmasq "$@"
}

check_bridge() {
    if do_brctl show | grep -E "^$1\\s" > /dev/null 2> /dev/null; then
      return 1
    else
      return 0
    fi
}

create_bridge() {
    do_brctl addbr "$1"
    echo "waitning br $1 up"
    while check_bridge "$1"; do
      sleep 0.5
      echo " repeating... (waiting $1)"
    done
    do_brctl show | grep "^$1"
    do_brctl stp "$1" off
    do_brctl setfd "$1" 0
    do_ifconfig "$1" "$GATEWAY" netmask 255.255.255.0 up
}

enable_ip_forward() {
    echo 1 | do_dd of=/proc/sys/net/ipv4/ip_forward > /dev/null
}

add_filter_rules() {
   if $MASQUERADE; then
      do_iptables -t nat -A POSTROUTING -s $NETWORK/24 -j MASQUERADE
   fi
   do_iptables -A FORWARD -o $BRIDGE -s $NETWORK/24 -j ACCEPT
   do_iptables -A FORWARD -o $BRIDGE -s $NETWORK/16 -j DROP
}

start_dnsmasq() {
    do_dnsmasq \
	--strict-order \
	--except-interface=lo \
	--interface=$BRIDGE \
	--listen-address=$GATEWAY \
	--bind-interfaces \
	--dhcp-range=$DHCPRANGE \
	--conf-file="" \
	--pid-file=/var/run/qemu-dnsmasq-$BRIDGE.pid \
	--dhcp-leasefile=/var/run/qemu-dnsmasq-$BRIDGE.leases \
	--dhcp-no-override \
	${TFTPROOT:+"--enable-tftp"} \
	${TFTPROOT:+"--tftp-root=$TFTPROOT"} \
	${BOOTP:+"--dhcp-boot=$BOOTP"}
}

setup_bridge_nat() {
    if check_bridge "$1" ; then
      create_bridge "$1"
      enable_ip_forward
      add_filter_rules "$1"
      start_dnsmasq "$1"
    fi
}

setup_bridge_vlan() {
    if check_bridge "$1" ; then
	create_bridge "$1"
	start_dnsmasq "$1"
    fi
}

setup_bridge_nat "$BRIDGE"

if test "$1" ; then
    do_ifconfig "$1" 0.0.0.0 up
    do_brctl addif "$BRIDGE" "$1"
fi

exit 0
