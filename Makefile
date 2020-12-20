PROGRAM = obluda

all: 
	ghc -O -o $(PROGRAM) $(PROGRAM).hs

