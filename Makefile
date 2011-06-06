GHC=ghc
GHCFLAGS=-O2 -fhpc

todos: *.hs
	$(GHC) $(GHCFLAGS) -fwarn-unused-imports -fwarn-unused-binds --make todos.hs

clean:
	find . -name \*.hi -delete
	find . -name \*.o -delete
	find . -name \*~ -delete
