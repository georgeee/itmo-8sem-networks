#!/bin/bash

cd ../anycast
mvn clean install

cd ../anycast-test
cp ../anycast/target/timesync-bundle.zip bundle

./launch.sh
