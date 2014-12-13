CC = gcc
HC = ghc
HSFLAGS = -optc -std=c11 -optc -Wall -optc -Wextra -optc -Wno-unused-parameter -optc -Wno-sign-compare -optc -O0 -optc -g -O0
CFLAGS = -g -Wall -std=c11 -O0
SOURCES := $(wildcard src/*.c)
OBJECTS := $(patsubst %.c,%.o,${SOURCES})
HSMODULES := src/lisp/EditorLisp src/lisp/Parser src/lisp/LispCore src/lisp/LispValues src/lisp/LispFunctions

edit : lisp ${OBJECTS}
	$(HC) --make -no-hs-main $(HSFLAGS) -o edit $(SOURCES) $(HSMODULES) -lncurses
%.o : %.c
	$(HC) -c $(HSFLAGS) $< -o $@
lisp :
	ghc -O0 src/lisp/*.hs
