#!/bin/bash

# Tabulate results of running benchmark suite in gp2 reference interpreter

outOne="$PWD/results_table_one.tex"
outAll="$PWD/results_table_all.tex"

function benchmarkName() {
	basename $1 | sed -e 'y/-_/  /'
}

function graphCounts() {
awk -v "bm=`basename $PWD`" -v "short=$2" 'BEGIN { f=0; u=0; n=0 ; i=0 ; rmin=0 ; rmax=0 ; t=0 ; gsub(/\.host.*\.d/, "", bm); gsub(/_/, " ", bm);} \
		 /^[0-9]* fail/ { f+=$1 } \
		 /^[0-9]* unfinished/ { u+=$1 } \
		 /^[0-9]* occurrence/ { n+=$1 ; i+=1 } \
		 /^Rule application bounds./ { match($6, "\\(([0-9]*),([0-9]*)\\)", bounds) ; rmin=bounds[1] ; rmax=bounds[2] } \
		 /^.*user .*system .*elapsed/ { match($0, "([0-9\\.]*)user ([0-9\\.]*)system", times) ; t=times[1]+times[2]; t=t>0.01 ? sprintf("%.2f", t) : "$<0.01$" } \
		 /Timed out after/ { failed=1 }
		 END { \
			 rng=(rmin==rmax) ? rmin : (rmin "-" rmax) ; \
			 if (short) {
				if (failed) { \
					printf " & %20s & - & $>5m$ & & - & - \\\\\n", bm ; exit 1
				} else { \
					printf " & %20s & %5s & %7s & & ", bm, rng, t \
				}
			 } else {
				if (failed) { \
					printf " & %20s & - & - & - & - & $>5m$ & & - & - \\\\\n", bm ; exit 1
				} else { \
					printf " & %20s & %6d & %9d & %5d & %5s & %7s & & ", bm, n, i, f, rng, t \
				}
			}
		 }' $1/test.log

}

function heapProfile() {
	awk 'BEGIN {h=0; l=0 } \
		/maximum residency/ { gsub(/,/, "", $1); l=$1/1024 } 
		/total memory in use/ { h = $2=="kB" ? $1 : $2=="MB" ? $1*1024 : $1*1024*1024 }
		 END { printf "%5d & %5d \\\\\n", h, l } ' $1/gc.prof
}

:>"$outOne"
:>"$outAll"

for d in */ ; do
	pushd $d || break
	benchmarkName `pwd` >> "$outOne"
	for d in *.host--one.d/ ; do 
		pushd $d >/dev/null || break
		graphCounts ./ 1 && heapProfile ./ # output short format
		#echo '                  \cline{2-12}'
		popd >/dev/null
	done >> "$outOne"
	echo '\hline' >> "$outOne"

	benchmarkName `pwd` >> "$outAll"
	for d in *.host.d/ ; do 
		pushd $d >/dev/null || break
		graphCounts ./ && heapProfile ./
		#echo '                  \cline{2-12}'
		popd >/dev/null
	done >> "$outAll"
	echo '\hline' >> "$outAll"

	popd
	shift
done
