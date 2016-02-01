ASSEMBLER32=gcc -static -nostdlib -m32 -g 
STEM=oilr_machine
TARGET=$(STEM)
# PROF_TARGET=$(STEM)-profile


all: $(TARGET)

sloc: $(TARGET).S
	cat $< | grep -v '^\s*$$' | grep -v '^\s*#[^a-z]' | grep -v '^\s*//' | wc -l

%: %.S
	$(ASSEMBLER32) -o $@ $<


