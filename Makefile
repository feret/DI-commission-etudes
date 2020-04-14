.DEFAULT_GOAL := all



.PHONY: all clean

all:
	dune build --only-packages sco-binaries
	cp _build/default/main/Scolarite.exe Scolarite
	ln -sf ./Scolarite $(HOME)/local/bin/
clean:
	dune clean
