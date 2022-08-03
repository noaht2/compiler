test.mmo : test.mms
	mmixal -x test.mms

test.mms : Main
	./Main
Main : *.hs
	ghc Main.hs

clean :
	rm -f *.mms *.mmo *.hi *.o Main *~
