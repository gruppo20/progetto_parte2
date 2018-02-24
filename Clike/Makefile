all: TestClike
	
bnfc: Clike.cf
	bnfc -haskell Clike.cf
	
ParClike.y: bnfc
	happy -gca ParClike.y
	
LexClike.x: bnfc ParClike.y
	alex -g LexClike.x
	
TestClike: ParClike.y LexClike.x
	ghc --make TestClike.hs -o TestClike
	
clean:
	rm *.hi *.o DocClike.txt
	
demo: test.txt
	./TestClike < test.txt
	
.PHONY: TestClike clean demo

