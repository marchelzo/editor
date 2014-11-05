#define _GNU_SOURCE

#include <stdlib.h>
#include <string.h>
#include <curses.h>

#include "buffer.h"
#include "gapbuffer.h"
#include "loadfile.h"
#include "state.h"

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
    if (f->numLines == 0 && f->lines[0] == NULL)
        return b;
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

void b_insertString(Buffer *b, const char *s)
{
    gb_insertString(b->line->content, s);
}

void b_cursesPrint(Buffer *b, int x, int y)
{
    LineNode *c = b->line;
    size_t currentLine = b->currentLine;
    while (b->currentLine > g_cb->yScroll) {
        b->line = b->line->prev;
        --b->currentLine;
    }
    for (int i = 0; i < g_termRows; ++i) {
        move(y + i, x);
        gb_cursesPrint(b->line->content, g_cb->xScroll, g_termCols - x);
        if (b->line->next)
            b->line = b->line->next;
        else
            break;
    }
    b->line = c;
    b->currentLine = currentLine;
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
    if (b->line->next == NULL)
        return;
    b->line = b->line->next;
    ++b->currentLine;
}

void b_cursesPositionCursor(Buffer *b, int xOff, int yOff)
{
    move(b->currentLine + yOff - g_cb->yScroll, gb_getPosition(b->line->content) + xOff - g_cb->xScroll);
}

void b_goToStart(Buffer *b)
{
    while (b->line->prev)
        b->line = b->line->prev;
    b->currentLine = 0;
    gb_goToStart(b->line->content);
}

int b_isEmpty(Buffer *b)
{
    return b->numLines == 1 && gb_length(b->line->content) == 0;
}

int b_nextLine(Buffer *b)
{
    if (b->line->next) {
        b->line = b->line->next;
        ++b->currentLine;
    } else return 1;
    return 0;
}

int b_prevLine(Buffer *b)
{
    if (b->line->prev) {
        b->line = b->line->prev;
        --b->currentLine;
    } else return 1;
    return 0;
}

void b_goToEOL(Buffer *b)
{
    gb_goToEnd(b->line->content);
}

void b_goToSOL(Buffer *b)
{
    gb_goToStart(b->line->content);
}

char *b_getCurrentLine(Buffer *b)
{
    return gb_cString(b->line->content);
}

void b_deleteCurrentLine(Buffer *b)
{
    /* return early if there are no other lines in the buffer */
    if (!(b->line->prev || b->line->next)) return;
    /* make a pointer to the current line so that we can free it after removing it from the buffer */
    LineNode *n = b->line;

    if (b->line->prev)
        b->line->prev->next = b->line->next;
    if (b->line->next)
        b->line->next->prev = b->line->prev;
    if (b->line->next)
        b->line = b->line->next;
    else {
        /* we were on the very last line, and so we are forced to go to the previous line */
        b->line = b->line->prev;
        --b->currentLine;
    }
    /* decrement the line count */
    --b->numLines;
    /* free the content of the deleted line */
    gb_free(n->content);

}

size_t b_columnNumber(Buffer *b)
{
    return (size_t) gb_getPosition(b->line->content);
}
