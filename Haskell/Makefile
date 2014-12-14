GHC=ghc -O2 -prof -fprof-auto -rtsopts


all : gp2 gp2c ViewGraph IsoChecker

gp2c : GP2c.hs
	$(GHC) -o $@ $<

gp2 : Main.hs
	$(GHC) -o $@ $<

% : %.hs
	$(GHC) $<

clean :
	rm -f *.o ViewGraph IsoChecker Main gp2

.PHONY : gp2 gp2c
