#!/bin/bash

grep '\(^Enter\|Bound\|bind op\)' "$1" | grep -n '\(^Entering\|bind op\)' | less

