#include <stdlib.h>
#include <string.h>
#include <curses.h>

#include "buffer.h"
#include "gapbuffer.h"

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

void b_insertLine(Buffer *b)
{
    LineNode *n = malloc(sizeof(LineNode));
    n->prev = b->line;
    n->next = b->line->next;
    b->line->next = n;
    n->content = gb_new();
    n->content->fst = b->line->content->snd;
    n->content->fsz = b->line->content->ssz;
    b->line->content->ssz = 0;
    b->line->content->snd = malloc(1);
    b->line = n;
    gb_position(n->content, n->content->fsz);
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
    int i = 0;
    while (b->line != NULL) {
        move(y + i, x);
        gb_cursesPrint(b->line->content);
        b->line = b->line->next;
        ++i;
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
    b->line->prev->content->fst = realloc(b->line->prev->content->fst,
                                          b->line->prev->content->fsz + b->line->content->ssz);
    memcpy(b->line->prev->content->fst + b->line->prev->content->fsz, b->line->content->snd, b->line->content->ssz);
    b->line->prev->next = b->line->next;
    b->line->prev->content->fsz += b->line->content->ssz;
    GapBuffer *temp = b->line->content;
    b->line = b->line->prev;
    gb_free(temp);
}
