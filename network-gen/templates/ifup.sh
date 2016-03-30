
b1=%b1%
b2=%b2%
b3=%b3%

ensure_dev_up ens4
ifconfig ens4 192.168.b1.1
ensure_dev_up ens5
ifconfig ens5 192.168.b2.2
ensure_dev_up ens6
ifconfig ens6 192.168.b3.1
