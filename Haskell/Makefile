GHC=ghc -O2 -prof -fprof-auto -rtsopts

TARGETS=gp2 gp2c ViewGraph IsoChecker

all : $(TARGETS)

gp2c : GP2c.hs
	$(GHC) -o $@ $<

gp2 : Main.hs
	$(GHC) -o $@ $<

% : %.hs
	$(GHC) $<

clean :
	rm -f *.o $(TARGETS)

.PHONY : gp2 gp2c
