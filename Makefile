yinsh.js: yinsh.hs
	rm -f yinsh.js
	hastec -Wall -fno-warn-unused-do-bind -fno-warn-missing-signatures -fno-warn-type-defaults yinsh.hs

doc: yinsh.hs
	rm -rf doc
	haddock -o doc -h yinsh.hs

opt: yinsh.hs
	rm yinsh.js
	hastec --opt-all yinsh.hs

clean:
	rm -f yinsh.js yinsh.hi yinsh.o
	rm -rf main doc
