yinsh.js: yinsh.hs
	hastec -Wall -fno-warn-unused-do-bind yinsh.hs

doc: yinsh.hs
	rm -r doc
	haddock -o doc -h yinsh.hs
