all : Main ;

Main : *.hs ;
	ghc Main.hs

clean : ;
	rm -f *.mms *.mmo *.hi *.o Main *~
