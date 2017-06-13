#!/bin/bash

function clean-tmp {
   cd /tmp/gp2 
   make clean
   cd -
}

if ./gp2 -p test/writerprog | grep -q 'writerprog is valid.'; then
   echo "PASS: Validation test passed."
else
   echo "FAIL: Validation test failed."
   exit 1
fi

./gp2 test/writerprog test/writer-helloworld

if [ -d /tmp/gp2 ] && [ -f /tmp/gp2/main.c ]; then
   echo "PASS: Writer program compiled successfully."
else
   echo "FAIL: Writer program failed to compile."
   clean-tmp
   exit 1
fi
  
echo "All tests passed!"
clean-tmp

