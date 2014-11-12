CC = gcc
CFLAGS = -g -std=c11 -O0
OBJECTS = main.o buffer.o gapbuffer.o editbuffer.o normal.o insert.o command.o quit.o loadfile.o bufwrite.o commandcompletion.o stringlist.o hashmap.c strdup.c

edit : $(OBJECTS)
	$(CC) $(CFLAGS) -o ../edit $(OBJECTS) -lncurses
%.o : %.c
	$(CC) -c $(CFLAGS) $< -o $@
