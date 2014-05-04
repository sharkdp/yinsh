Frontend.js: Frontend.hs Yinsh.hs Floyd.hs
	rm -f Frontend.js
	hastec -Wall -fno-warn-unused-do-bind -fno-warn-missing-signatures -fno-warn-type-defaults Frontend.hs

doc: Frontend.hs Yinsh.hs Floyd.hs
	rm -rf doc
	haddock -o doc -h Yinsh.hs Floyd.hs Frontend.hs

opt: Frontend.hs Yinsh.hs Floyd.hs
	rm Frontend.js
	hastec --opt-all Frontend.hs

clean:
	rm -f Frontend.js *.hi *.o
	rm -rf main doc
