all: TestZproto
	
bnfc: Zproto.cf
	bnfc -haskell Zproto.cf
	
ParZproto.y: bnfc
	happy -gca ParZproto.y
	
LexZproto.x: bnfc ParZproto.y
	alex -g LexZproto.x
	
TestZproto: ParZproto.y LexZproto.x
	ghc --make TestZproto.hs -o TestZproto
	
clean:
	rm *.hi *.o DocZproto.txt
	
demo: test.txt
	./TestZproto < test.txt
	
.PHONY: TestZproto clean demo
