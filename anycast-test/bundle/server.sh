#!/bin/bash

ifconfig lo inet6 add fc00:192:168:215::1
java -jar timesync/timesync.jar -s
