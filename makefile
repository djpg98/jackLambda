COMPILAR	= ghc -c

LINKEAR		= ghc

LAMBDA		= JackLambda

OBJETOS		= Cartas.o JackLambda.o

.PHONY: all clean

all: $(LAMBDA)

clean:
	rm $(OBJETOS)
	rm $(LAMBDA)

Cartas.o: Cartas.hs
	$(COMPILAR) Cartas.hs -o Cartas.o

$(LAMBDA): JackLambda.hs Cartas.o
	$(COMPILAR) JackLambda.hs -o JackLambda.o
	$(LINKEAR) $(OBJETOS) -o $(LAMBDA)