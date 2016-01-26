#!/bin/bash

GP2C_OPTS=""
GP2C="../gp2c $GP2C_OPTS"

TESTS="bidi bidi2 colour looped-rule-set negatives rooted-2-colouring root rule-set transitive_closure"



function clean_test_dir () {
	
}


for t in $TESTS ; do
	$GP2C $t.gp2 $t.host 
done

