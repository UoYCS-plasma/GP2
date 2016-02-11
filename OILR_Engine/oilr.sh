#!/bin/bash

if [ "`type -t $0`"="file" ]; then
	SCR_PATH="`type -p $0`"
else
	SCR_PATH="`readlink $0`"
fi

OILR_DIR="`dirname $SCR_PATH`"
if [ -z "$OILR_DIR" ]; then
	echo "You should symlink oilr.sh"
	echo "into your path"
	exit 1
fi

OILR="$OILR_DIR/oilr_machine"
OILR_LIB="$OILR_DIR/lib/*"

exec $OILR $OILR_LIB $@


