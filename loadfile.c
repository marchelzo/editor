#include "loadfile.h"

LoadedFile *loadFile(FILE *f)
{
    int c;
    size_t memForLinePtrs = 100;
    char **lns = malloc(100 * sizeof(char*));
    size_t *lens = malloc(100 * sizeof(size_t));
    size_t numLines = 0;
    LoadedFile *lf = malloc(sizeof(LoadedFile));
    while (1) {
        size_t len = 0;
        size_t mem = 100;
        char *l = malloc(100);
        while ((c = fgetc(f))) {
            if (c == '\n') break;
            if (c == EOF) goto lastline;
            if (len == mem) {
                l = realloc(l, mem * 2);
                mem *= 2;
            }
            l[len] = c;
            ++len;
        }
        if (memForLinePtrs - 2 == numLines) {
            lns = realloc(lns, memForLinePtrs / 2 * 3 * sizeof(char*));
            lens = realloc(lens, memForLinePtrs / 2 * 3 * sizeof(size_t));
            memForLinePtrs = memForLinePtrs / 2 * 3;
        }
        lns[numLines] = l;
        lens[numLines] = len;
        ++numLines;
        continue;
    lastline:
        if (len > 0) {
            lns[numLines] = l;
            lens[numLines] = len;
            ++numLines;
        }
        goto end;
    }
    end:
    lf->lines = lns;
    lf->lengths = lens;
    lf->numLines = numLines;
    return lf;
}
