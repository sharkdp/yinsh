yinsh.js: yinsh.hs
	hastec -Wall -fno-warn-unused-do-bind yinsh.hs

doc: yinsh.hs
	rm -rf doc
	haddock -o doc -h yinsh.hs
