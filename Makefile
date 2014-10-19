WARNFLAGS=-Wall -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-orphans
OUTFLAGS=-isrc -odir build -hidir build -outputdir build
GHCFLAGS=$(WARNFLAGS) $(OUTFLAGS)
SRC=src/Yinsh.hs src/AI.hs src/Floyd.hs src/RaiCharles.hs
SRC_FRONTEND=$(SRC) src/frontend.hs
SRC_CLI=$(SRC) src/match.hs
SRC_BACKEND=$(SRC) src/backend.hs

frontend.js: $(SRC_FRONTEND)
	rm -f frontend.js
	hastec $(GHCFLAGS) -O2 src/frontend.hs -o frontend.js

docs: $(SRC_FRONTEND) info/turn-structure.svg
	rm -rf docs
	mkdir docs
	cp info/turn-structure.svg docs
	haddock -o docs -h $(SRC_FRONTEND) || rm -r docs

tests: $(SRC_FRONTEND)
	doctest $(SRC_FRONTEND)

opt: $(SRC_FRONTEND)
	rm -f frontend.js
	hastec $(GHCFLAGS) --opt-all src/frontend.hs -o frontend.js

match: $(SRC_CLI)
	ghc $(GHCFLAGS) -O2 -threaded src/match.hs -o match

match-prof: $(SRC_CLI)
	ghc $(GHCFLAGS) -prof -auto-all -O2 src/match.hs -o match-prof
	./match-prof -s +RTS -p
	mv match-prof.prof prof

backend: $(SRC_BACKEND)
	ghc $(GHCFLAGS) -O2 src/backend.hs -o backend

clean:
	rm -rf build
	rm -rf main
	# rm -rf docs
	rm -f frontend.js
	rm -f match match.prof
