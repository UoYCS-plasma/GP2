#!/bin/bash

awk '
	BEGIN { print "digraph { {" }
	/^[BSF]/ { print "}" ; id=substr($0, 2) ; uniq=uniq+1 }
	/^OILR/ { print "subgraph cluster_" $0 "_" id "_" uniq, "{\n\tlabel=" $0 "\n\tgraph [ style=solid ] " }
	/^[0-9]{1,}$/ { print "\tn" uniq "_" $0 }
	/^![0-9]{1,}$/ { print "\tn" uniq "_" substr($0, 2), "[ color=blue ]" }
	/^\?[0-9]{1,}$/ { print "\tn" uniq "_" substr($0, 2), "[ color=red ]" }
	/^[0-9]{1,}->[0-9]{1,}/ { print "\tn" uniq "_" $1 "->n" uniq "_" $2 }
	/^![0-9]{1,}->[0-9]{1,}/ { print "\tn" uniq "_" substr($1, 2) "->n" uniq "_" $2, "[ color=blue ]" }
	/^\?[0-9]{1,}->[0-9]{1,}/ { print "\tn" uniq "_" substr($1, 2) "->n" uniq "_" $2, "[ color=red ]" }
	// {}
	END { print "}" }
' RS='[ \n\t]' FS='->'
echo "}"
