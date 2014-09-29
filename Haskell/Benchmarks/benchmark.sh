#!/bin/bash

GPOPTS=""
GP="time ../../../Main"

for b in */ ; do
	cd "$b"
	for host in *.host ; do
		rm -rf "$host.d"
		mkdir "$host.d"
		pushd "$host.d"
		$GP ../*.gp2 ../"$host" >> ../test.log
		popd
	done
done
