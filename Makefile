WARNFLAGS=-Wall -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-orphans
OUTFLAGS=-isrc -odir build -hidir build -outputdir build
EXTFLAGS=-XFlexibleInstances -XUndecidableInstances
GHCFLAGS=$(WARNFLAGS) $(OUTFLAGS) $(EXTFLAGS)
SRC=src/Yinsh.hs src/Floyd.hs src/AI.hs src/RandomAI.hs
SRC_FRONTEND=$(SRC) src/Frontend.hs
SRC_CLI=$(SRC) src/match.hs

Frontend.js: $(SRC_FRONTEND)
	rm -f Frontend.js
	hastec $(GHCFLAGS) -O2 src/Frontend.hs -o Frontend.js

docs: $(SRC_FRONTEND) info/turn-structure.svg
	rm -rf docs
	mkdir docs
	cp info/turn-structure.svg docs
	haddock -o docs -h $(SRC_FRONTEND)

tests: $(SRC_FRONTEND)
	doctest $(SRC_FRONTEND)

opt: $(SRC_FRONTEND)
	rm -f Frontend.js
	hastec $(OUTFLAGS) --opt-all src/Frontend.hs -o Frontend.js

match: $(SRC_CLI)
	ghc $(GHCFLAGS) -O2 src/match.hs -o match

prof: $(SRC_CLI)
	ghc $(GHCFLAGS) -prof -auto-all -O2 src/match.hs -o match
	./match +RTS -p
	mv match.prof prof

clean:
	rm -rf build
	rm -rf main
	rm -rf docs
	rm -f Frontend.js
	rm -f match match.prof
