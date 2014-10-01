#!/bin/bash

GPOPTS=""
GP="time -o test.log -a ../../../Main"

BMS="$@"

if [ -z "$@" ] ; then BMS="*/" ; fi

for b in $BMS ; do
	pushd "$b"
	prog=`ls *.gp2`
	for host in `ls *.host` ; do
		rm -rf "$host.d"
		mkdir "$host.d"
		pushd "$host.d"
		$GP ../"$prog" ../"$host" 20 >> test.log
		popd
	done
	popd
done
