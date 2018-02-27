all: compile Clike
	
ParClike: ParClike.y LexClike.hs
	happy -gca ParClike.y
	
LexClike: LexClike.x 
	alex -g LexClike.x
	
compile: LexClike ParClike
	
TestClike: compile
	ghc --make TestClike.hs -o TestClike
	
Clike: compile
	ghc --make Clike.hs -o Clike
	
clean:
	rm *.hi *.o
	
demo: Clike test.txt
	./Clike test.txt
	
.PHONY: compile Clike clean demo

