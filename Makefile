GHC=ghc
GHCFLAGS=-O2

todos: *.hs
	$(GHC) $(GHCFLAGS) --make todos.hs

clean:
	rm -f *.hi *.o *~
