#!/bin/bash

for p in Tests/*.oilr ; do
	echo ============================================
	echo "   $p"
	echo ============================================
	./oilr.sh testLib.oilr  $p || exit 1
done
