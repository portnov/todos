GHC=ghc
GHCFLAGS=-O2 -fhpc

todos: *.hs
	$(GHC) $(GHCFLAGS) --make todos.hs

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
	find . -name \*~ -delete
