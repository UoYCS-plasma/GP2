ASSEMBLER32=gcc -static -nostdlib -m32 -g 
STEM=oilr_machine
TARGET=./$(STEM)
ISOCHKR=../Haskell/IsoChecker
# PROF_TARGET=$(STEM)-profile


all: $(TARGET) tests test-progs

tests: $(TARGET) Tests/*.oilr runtests.sh
	./runtests.sh

test-progs: $(ISOCHKR) runprogs.sh
	./runprogs.sh

graph-test: lib/*.oilr testLib.oilr test-graphLib.oilr
	$(TARGET) $^

sloc: $(TARGET).S
	cat $< | grep -v '^\s*$$' | grep -v '^\s*#[^a-z]' | grep -v '^\s*//' | wc -l

%: %.S
	$(ASSEMBLER32) -o $@ $<


