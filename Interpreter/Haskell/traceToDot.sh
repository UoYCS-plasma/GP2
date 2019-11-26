#!/bin/bash

[ -z "$1" ] && echo "Usage: $0 <destDir>  < tracefile" && exit

[ ! -d "$1" ] && mkdir -p "$1"

awk '
function nodeId(nid) {
	return "n" uniq "_" nid;
}
function format(type) {
	if (type == "!") {
		return "[ fillcolor=black ; color=black; weight=3 ]";
	} else if (type == "?") {
		return "[ fillcolor=grey ; color=black; weight=3 ]";
	}
}
function drawNode(n,   c) {
	c = substr(n, 1, 1);
	if (c == "!" || c == "?") {
		print "\t" nodeId(substr(n,2)), format(c) > outFile
	} else {
		print "\t" nodeId(n) > outFile
	}
}
function drawEdge(s, t,   c) {
	c = substr(s, 1, 1);
	if (c == "!" || c == "?") {
		print "\t" nodeId(substr(s, 2)) "->" nodeId(t), format(c) > outFile
	} else {
		print "\t" nodeId(s) "->" nodeId(t) > outFile
	}
}
function startGraph(title) {
	print "digraph { sortv=" uniq "\n\tgraph [ label=" title "_" uniq "; labelloc=b; style=solid ]\n\tnode [ color=grey; style=filled; fillcolor=white; label=\"\"; shape=circle ]\n\tedge [ color=grey; ]" > outFile
}
function endGraph() {
	print "}" > outFile
}
	/^[BSF]/ { if (outFile) { endGraph() } ; id=substr($0, 2) ; uniq=uniq+1 }
	/^OILR/ { outFile=sprintf("%s/%06d_%d_%s.d", destDir, uniq, id, $0); startGraph($0) }
	/^[\?!]?[0-9]{1,}$/ { drawNode($0) }
	/^[\?!]?[0-9]{1,}->[0-9]{1,}/ { drawEdge($1,$2) }
	// {}
	END { print "}" > outFile }
' RS='[ \n\t]' FS='->' destDir="$1"

cd "$1"
for g in *.d; do echo ${g}ot ; neato $g > ${g}ot && rm $g ; sleep 0.01 ; done

### This is how to produce a grid of steps from the resulting graphs
#  gvpack -array_u10 000{0,1,3}* | neato -n2 -Tx11
