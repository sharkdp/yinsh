Frontend.js: Frontend.hs Yinsh.hs Floyd.hs
	rm -f Frontend.js
	hastec -O2 -Wall -fno-warn-unused-do-bind -fno-warn-missing-signatures -fno-warn-type-defaults Frontend.hs

doc: Frontend.hs Yinsh.hs Floyd.hs
	rm -rf doc
	haddock -o doc -h Yinsh.hs Floyd.hs Frontend.hs

opt: Frontend.hs Yinsh.hs Floyd.hs
	rm -f Frontend.js
	hastec --opt-all Frontend.hs

prof: Yinsh.hs Floyd.hs profileYinsh.hs
	ghc -prof -auto-all -O2 profileYinsh.hs

cli: Yinsh.hs Floyd.hs profileYinsh.hs
	ghc -O2 -Wall -fno-warn-unused-do-bind -fno-warn-missing-signatures -fno-warn-type-defaults profileYinsh.hs

matchAI: Yinsh.hs Floyd.hs matchAI.hs
	ghc -O2 -Wall -fno-warn-unused-do-bind -fno-warn-missing-signatures -fno-warn-type-defaults matchAI.hs

clean:
	rm -f Frontend.js *.hi *.o
	rm -rf main doc
