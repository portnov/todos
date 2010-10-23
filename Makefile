GHC=ghc
GHCFLAGS=-O2

todos: *.hs
	$(GHC) $(GHCFLAGS) --make todos.hs

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
	find . -name \*~ -delete
