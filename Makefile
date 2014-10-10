WARNFLAGS=-Wall -fno-warn-unused-do-bind -fno-warn-missing-signatures -fno-warn-type-defaults
OUTFLAGS=-isrc -odir build -hidir build -outputdir build
GHCFLAGS=$(WARNFLAGS) $(OUTFLAGS)
SRC=src/Yinsh.hs src/Floyd.hs
SRC_FRONTEND=$(SRC) src/Frontend.hs
SRC_CLI=$(SRC) src/match.hs

Frontend.js: $(SRC_FRONTEND)
	rm -f Frontend.js
	hastec $(GHCFLAGS) -O2 src/Frontend.hs -o Frontend.js

doc: $(SRC)
	rm -rf doc
	haddock -o doc -h $(SRC)

opt: $(SRC_FRONTEND)
	rm -f Frontend.js
	hastec $(OUTFLAGS) --opt-all src/Frontend.hs -o Frontend.js

match: $(SRC_CLI)
	ghc $(GHCFLAGS) -O2 src/match.hs -o match

prof: $(SRC_CLI)
	ghc $(OUTFLAGS) -prof -auto-all -O2 src/match.hs -o match

clean:
	rm -rf build
	rm -rf main doc
	rm -f Frontend.js
	rm -f match
