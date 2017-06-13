#!/bin/bash -x

function clean-tmp {
   cd /tmp/gp2 
   make clean
   cd -
}

./gp2 test/writerprog
cp test/writer-helloworld /tmp/gp2/writer-helloworld
cd /tmp/gp2 && make

if ./gp2run writer-helloworld | grep -q 'Output graph saved to file gp2.output'; then
   echo "PASS: Writer program built and executed successfully."
else
   echo "FAIL: Writer program produced incorrect output."
   clean-tmp
   exit 1
fi

if cat gp2.output | grep -q '[ (0, "Hello" : "world") | ]'; then
   echo "PASS: Writer program produced correct output."
else
   echo "FAIL: Writer program produced incorrect output."
   clean-tmp
   exit 1
fi

