#!/bin/bash

TIMEOUT=5m
MAXAPPS=1000000
MAXGRAPHS=10

while getopts ":a:g:t:" opt ; do
	case "$opt" in
		a)
			MAXAPPS="$OPTARG"
			;;
		g)
			MAXGRAPHS="$OPTARG"
			;;
		t)
			TIMEOUT="$OPTARG"
			;;
		*)
			echo Unrecognised arg: -$OPTARG
			exit
			;;
	esac
done
shift $(( $OPTIND - 1 ))  # only if every opt has an arg!

BMS="$@"

if [ -z "$@" ] ; then BMS="*/" ; fi

default="[0m"
bold="[1m"
red="[31m"
amber="[33m"
green="[32m"

for mode in "--one" "" ; do
	GPOPT="$mode"
	[ "$mode" = "--no-iso" ] && GPOPT="--no-iso=$MAXGRAPHS"
	GP="time ../../../gp2 +RTS -p -sgc.prof -RTS $GPOPT "
	printf "=================================================% 10s ===\n" $mode
	for b in $BMS ; do
		pushd "$b" > /dev/null
		prog=`ls *.gp2`
		for host in `ls *.host` ; do
			wd="$host$mode.d"
			rm -rf "$wd"
			mkdir "$wd"
			pushd "$wd" > /dev/null
			echo -e "=== $bold$wd$default"
			echo "$wd" >> test.log
			echo "$GP" >> test.log
			echo "Max allowed time is: $TIMEOUT" >> test.log
			/usr/bin/timeout -s HUP --foreground $TIMEOUT $GP ../"$prog" ../"$host" $MAXAPPS >> test.log 2>&1
			state=$?
			killall -HUP gp2 2>/dev/null  # clean-up possibly unterminated gp2 instances
			if [ "$state" = "124" ] ; then
				echo "$amber	Timed out$default"
				echo "*** Timed out after" $TIMEOUT >> test.log
			elif [ "$state" = "0" ] ; then
				echo "$green	Success$default"
				echo "*** Success" >> test.log
			else
				echo "$red	Failure$default"
				echo "*** Failure" >> test.log
				tail -5 test.log
			fi

			popd > /dev/null
		done
		popd > /dev/null
	done
done
