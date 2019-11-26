#!/bin/bash


binds=`grep -c '^B' "$1"`
echo Program completed in $binds bind operations

# show rules found in log file
rules=`cut -d ' ' -f 2 $1 | sort -u`
echo Rules: $rules

for r in $rules ; do
	printf " - %s succeeded %d times and failed %d times\n" $r `grep -c "^S[0-9]* $r :" $1` `grep -c "^F[0-9]* $r :" $1`
done

calls=`cut -d: -f1 $1 | sed 's/^.\([0-9]*\) .*/\1/' | sort -h -u`
for c in $calls ; do
	name=`grep -m1 "^.$c " $1 | cut -d ' ' -f 2 `
	bops=`grep -c "^B$c " $1`
	result=`grep "^[SF]$c " $1 | sed -e 's/^S.*/success/' -e 's/^F.*/failure/'`
	printf "\t - %d %s %d bind-ops to %s\n" $c $name $bops $result
done
