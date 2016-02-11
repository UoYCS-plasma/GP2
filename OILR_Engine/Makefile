ASSEMBLER32=gcc -static -nostdlib -m32 -g 
STEM=oilr_machine
TARGET=./$(STEM)
ISOCHKR=../Haskell/IsoChecker
# PROF_TARGET=$(STEM)-profile


all: $(TARGET) test

test: core-test graph-test test-progs

test-progs: $(ISOCHKR) runprogs.sh
	./runprogs.sh

core-test: lib/*.oilr testLib.oilr test-core.oilr
	$(TARGET) $^

graph-test: lib/*.oilr testLib.oilr test-graphLib.oilr
	$(TARGET) $^

sloc: $(TARGET).S
	cat $< | grep -v '^\s*$$' | grep -v '^\s*#[^a-z]' | grep -v '^\s*//' | wc -l

%: %.S
	$(ASSEMBLER32) -o $@ $<


