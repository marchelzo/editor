#include <stdlib.h>
#include <string.h>
#include <curses.h>

#include "gapbuffer.h"

GapBuffer *gb_new(void)
{
    GapBuffer *gb = malloc(sizeof(GapBuffer));
    gb->fst = malloc(1);
    gb->snd = malloc(1);
    gb->fsz = 0;
    gb->ssz = 0;
}

GapBuffer *gb_fromCString(char *s, size_t n)
{
    GapBuffer *b = gb_new();
    b->ssz = (size_t) n;
    b->snd = s;
}

void gb_position(GapBuffer *gb, int p)
{
    if (p < 0 || p > gb->fsz + gb->ssz || p == gb->fsz)
        return;
    if (p > gb->fsz) {
        while (p > gb->fsz) {
            gb->fst = realloc(gb->fst, gb->fsz + 1);
            gb->fst[gb->fsz] = gb->snd[gb->ssz - 1];
            gb->snd = realloc(gb->snd, gb->ssz - 1);
            ++gb->fsz;
            --gb->ssz;
        }
    } else {
        while (p < gb->fsz) {
            gb->snd = realloc(gb->snd, gb->ssz + 1);
            gb->snd[gb->ssz] = gb->fst[gb->fsz - 1];
            gb->fst = realloc(gb->fst, gb->fsz - 1);
            ++gb->ssz;
            --gb->fsz;
        }
    }

}

int gb_forcePosition(GapBuffer *gb, int p)
{
    if (p < 0)
        p = 0;
    if (p > gb->ssz + gb->fsz)
        p = gb->ssz + gb->fsz;
    gb_position(gb, p);
    return p;
}

int gb_getPosition(GapBuffer *gb)
{
    return gb->fsz;
}

void gb_insertChar(GapBuffer *gb, char c)
{
    gb->fst = realloc(gb->fst, gb->fsz + 1);
    gb->fst[gb->fsz] = c;
    ++gb->fsz;
}

void gb_insertString(GapBuffer *gb, char *s)
{
    size_t len = strlen(s);
    gb->fst = realloc(gb->fst, gb->fsz + len);
    for (int i = 0; i < len; ++i) {
        gb->fst[gb->fsz + i] = s[i];
    }
    gb->fsz += len;
}

char *gb_substring(GapBuffer *gb, int begin, int end)
{
    size_t len = (size_t) (end - begin);
    char *s = malloc(len + 1);
    int i = 0;
    int k = begin;
    while (i < len && k < gb->fsz) {
        s[i] = gb->fst[k];
        ++i;
        ++k;
    }
    k = 0;
    while (i < len && k < gb->ssz) {
        s[i] = gb->snd[k];
        ++i;
        ++k;
    }
    s[len] = '\0';
    return s;
}

size_t gb_length(GapBuffer *gb)
{
    return gb->fsz + gb->ssz;
}

void gb_delete(GapBuffer *gb, size_t n)
{
    size_t numDeleted = MIN(gb->fsz, n);
    gb->fst = realloc(gb->fst, gb->fsz - numDeleted);
    gb->fsz -= numDeleted;
}

char *gb_cString(GapBuffer *gb)
{
    return gb_substring(gb, 0, gb_length(gb));
}

void gb_cursesPrint(GapBuffer *b)
{
    for (int i = 0; i < b->fsz; ++i)
        addch(b->fst[i]);
    for (int i = b->ssz - 1; i >= 0; --i)
        addch(b->snd[i]);
}

void gb_goToEnd(GapBuffer *b)
{
    gb_position(b, gb_length(b));
}

void gb_goToStart(GapBuffer *b)
{
    gb_position(b, 0);
}

void gb_free(GapBuffer *gb)
{
    free(gb->fst);
    free(gb->snd);
    free(gb);
}

void gb_moveLeft(GapBuffer *b, int n)
{
    gb_position(b, MAX(0, (int)b->fsz - n));
}

void gb_moveRight(GapBuffer *b, int n)
{
    gb_position(b, MIN(b->fsz + b->ssz, b->fsz + n));
}
