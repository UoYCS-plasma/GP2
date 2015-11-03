GHC=ghc -O2
GHC_PROF=$(GHC) -prof -fprof-auto -rtsopts

TARGETS=gp2 gp2c ViewGraph IsoChecker

all : tags $(TARGETS)

gp2c : GP2c.hs OILR3/CRuntime.hs
	$(GHC) -o $@ $<

OILR3/CRuntime.hs : OILR3/oilrrt.c
	touch OILR3/CRuntime.hs

gp2 : Main.hs
	$(GHC_PROF) -o $@ $<

% : %.hs
	$(GHC) $<

tags :
	echo ":ctags" | ghc -v0 Main.hs

clean :
	rm -f *.o OILR3/*.o $(TARGETS)

sloc : OILR3/oilrrt.c
	grep -v '\s*//' $< | grep -v '^\s*$$' | wc -l

.PHONY : gp2 gp2c
