GHC=ghc -O2 -prof


all : gp2 ViewGraph IsoChecker

gp2 : Main.hs
	$(GHC) -o $@ $<

% : %.hs
	$(GHC) $<

clean :
	rm -f *.o ViewGraph IsoChecker Main gp2
