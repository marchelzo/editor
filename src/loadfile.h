#include <stdlib.h>
#include <stdio.h>

typedef struct {
    char **lines;
    size_t *lengths;
    size_t numLines;
} LoadedFile;

LoadedFile *loadFile(FILE *f);
