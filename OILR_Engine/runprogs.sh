#!/bin/bash

for p in Progs/*.oilr ; do
	./oilr.sh $p > /tmp/oilr.out
	../Haskell/IsoChecker $p.out /tmp/oilr.out || exit 1
done
