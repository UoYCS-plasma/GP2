#!/bin/bash


fail () {
	echo
	cat /tmp/oilr.out
	exit 1
}

for p in Progs/*.oilr ; do
	echo -ne "$p\n  "
	./oilr.sh $p > /tmp/oilr.out  || fail
	../Haskell/IsoChecker $p.out /tmp/oilr.out || exit 1
done
