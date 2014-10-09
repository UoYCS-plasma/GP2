GHC=ghc -O2 -prof -fprof-auto -rtsopts


all : gp2 ViewGraph IsoChecker

gp2 : Main.hs
	$(GHC) -o $@ $<

% : %.hs
	$(GHC) $<

clean :
	rm -f *.o ViewGraph IsoChecker Main gp2
