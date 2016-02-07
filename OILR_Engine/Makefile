ASSEMBLER32=gcc -static -nostdlib -m32 -g 
STEM=oilr_machine
TARGET=./$(STEM)
# PROF_TARGET=$(STEM)-profile


all: $(TARGET) test


test: core-test graph-test

core-test: core.oilr testLib.oilr test-core.oilr
	$(TARGET) $^

graph-test: core.oilr graphLib.oilr testLib.oilr test-graphLib.oilr
	$(TARGET) $^

sloc: $(TARGET).S
	cat $< | grep -v '^\s*$$' | grep -v '^\s*#[^a-z]' | grep -v '^\s*//' | wc -l

%: %.S
	$(ASSEMBLER32) -o $@ $<


