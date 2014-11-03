#define _GNU_SOURCE

#include <stdlib.h>
#include <string.h>
#include <curses.h>

#include "buffer.h"
#include "gapbuffer.h"
#include "loadfile.h"

static void reverseBytes(void *start, int size) {
    if (size == 0) return;
    unsigned char *lo = start;
    unsigned char *hi = start + size - 1;
    unsigned char swap;
    while (lo < hi) {
        swap = *lo;
        *lo++ = *hi;
        *hi-- = swap;
    }
}

Buffer *b_new()
{
    Buffer *b = malloc(sizeof(Buffer));
    b->line = malloc(sizeof(LineNode));
    b->line->content = gb_new();
    b->line->prev = NULL;
    b->line->next = NULL;
    b->numLines = 1;
    b->currentLine = 0;
}

Buffer *b_fromFile(FILE *fp)
{
    Buffer *b = b_new();
    LoadedFile *f = loadFile(fp);
    b->numLines = f->numLines;
    for (int i = 0; i < f->numLines; ++i) {
        b->line->content->fsz = f->lengths[i];
        b->line->content->fst = f->lines[i];
        b->line->next = malloc(sizeof(LineNode));
        b->line->next->prev = b->line;
        b->line->next->next = NULL;
        b->line->next->content = gb_new();
        b->line = b->line->next;
    }
    b->line = b->line->prev;
    b->line->next = NULL;
    b_goToStart(b);
    return b;
}

void b_insertLine(Buffer *b)
{
    LineNode *n = malloc(sizeof(LineNode));
    n->prev = b->line;
    n->next = b->line->next;
    if (b->line->next != NULL)
        b->line->next->prev = n;
    b->line->next = n;
    n->content = gb_new();
    n->content->fst = b->line->content->snd;
    n->content->fsz = b->line->content->ssz;
    /* reverse the bytes in the new line, to account for snd being backwards */
    reverseBytes(n->content->fst, n->content->fsz);
    gb_goToStart(n->content);
    b->line->content->ssz = 0;
    b->line->content->snd = malloc(1);
    b->line = n;
    gb_position(n->content, n->content->fsz);
    ++b->numLines;
    ++b->currentLine;
}

void b_insertChar(Buffer *b, char c)
{
    if (c == '\n')
        b_insertLine(b);
    else
        gb_insertChar(b->line->content, c);
}

void b_cursesPrint(Buffer *b, int x, int y)
{
    LineNode *c = b->line;
    while (b->line->prev != NULL)
        b->line = b->line->prev;
    for (int i = 0; i < b->numLines; ++i) {
        move(y + i, x);
        gb_cursesPrint(b->line->content);
        b->line = b->line->next;
    }
    b->line = c;
}

void b_backspace(Buffer *b)
{
    if (b->line->content->fsz == 0) {
        b_joinLine(b);
    } else {
        gb_delete(b->line->content, 1);
    }
}

void b_joinLine(Buffer *b)
{
    if (b->line->prev == NULL)
        return;
    gb_goToEnd(b->line->prev->content);
    b->line->prev->content->snd = b->line->content->snd;
    b->line->prev->next = b->line->next;
    if (b->line->next != NULL)
        b->line->next->prev = b->line->prev;
    b->line->prev->content->ssz = b->line->content->ssz;
    GapBuffer *temp = b->line->content;
    b->line = b->line->prev;
    free(temp);
    --b->numLines;
    --b->currentLine;
}

void b_cursorRight(Buffer *b)
{
    gb_moveRight(b->line->content, 1);
}

void b_cursorLeft(Buffer *b)
{
    gb_moveLeft(b->line->content, 1);
}

void b_cursorUp(Buffer *b)
{
    // TODO: refactor - add scroll(int) function instead of hard coding bounds checking here
    if (b->line->prev == NULL)
        return;
    b->line = b->line->prev;
    --b->currentLine;
}

void b_cursorDown(Buffer *b)
{
    // TODO: refactor - add scroll(int) function instead of hard coding bounds checking here
    if (b->line->next == NULL) {
        return;
    }
    b->line = b->line->next;
    ++b->currentLine;
}

void b_cursesPositionCursor(Buffer *b, int xOff, int yOff)
{
    move(b->currentLine + yOff, gb_getPosition(b->line->content) + xOff);
}

void b_goToStart(Buffer *b)
{
    while (b->line->prev)
        b->line = b->line->prev;
    b->currentLine = 0;
    gb_goToStart(b->line->content);
}
