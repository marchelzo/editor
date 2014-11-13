CC = gcc
CFLAGS = -g -Wall -std=c11 -O3
SOURCES := $(wildcard src/*.c)
OBJECTS := $(patsubst %.c,%.o,${SOURCES})

edit : ${OBJECTS}
	$(CC) $(CFLAGS) -o edit $(OBJECTS) -lncurses
%.o : %.c
	$(CC) -c $(CFLAGS) $< -o $@
