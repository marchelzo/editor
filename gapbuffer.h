#ifndef __GAPBUFFER_H__
#define __GAPBUFFER_H__

#include <stdlib.h>

#define MIN(a,b) (((a) > (b)) ? (b) : (a))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

typedef struct {
    char *fst;
    char *snd;
    size_t fsz;
    size_t ssz;
} GapBuffer;

GapBuffer *gb_new(void);

GapBuffer *gb_fromCString(char *s, size_t n);

void gb_position(GapBuffer *gb, int p);

int gb_forcePosition(GapBuffer*, int p);

int gb_getPosition(GapBuffer *gb);

void gb_insertChar(GapBuffer *gb, char c);

void gb_insertString(GapBuffer *gb, const char *s);

char *gb_substring(GapBuffer *gb, int begin, int end);

size_t gb_length(GapBuffer *gb);

char *gb_cString(GapBuffer *gb);

void gb_delete(GapBuffer *gb, size_t);

void gb_goToEnd(GapBuffer *gb);

void gb_goToStart(GapBuffer *gb);

void gb_cursesPrint(GapBuffer *gb, int start, int max);

void gb_free(GapBuffer *gb);

void gb_moveLeft(GapBuffer*, int n);

void gb_moveRight(GapBuffer*, int n);


#endif
