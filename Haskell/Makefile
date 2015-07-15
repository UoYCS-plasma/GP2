GHC=ghc -O2 -prof -fprof-auto -rtsopts

TARGETS=gp2 gp2c ViewGraph IsoChecker

all : tags $(TARGETS)

gp2c : GP2c.hs
	$(GHC) -o $@ $<

gp2 : Main.hs
	$(GHC) -o $@ $<

% : %.hs
	$(GHC) $<

tags :
	echo ":ctags" | ghc -v0 Main.hs

clean :
	rm -f *.o OilrMachine/*.o $(TARGETS)

.PHONY : gp2 gp2c
