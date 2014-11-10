#include <stdlib.h>
#include <string.h>
#include <curses.h>

#include "buffer.h"
#include "gapbuffer.h"
#include "loadfile.h"
#include "state.h"

static unsigned char contains(char *haystack, char needle)
{
    while (*haystack)
        if (*haystack++ == needle)
            return 1;
    return 0;
}

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
    if (f->numLines == 0)
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

void b_cursesDraw(Buffer *b, int x, int y)
{
    LineNode *c = b->line;
    size_t currentLine = b->currentLine;
    while (b->currentLine > g_cb->yScroll) {
        b->line = b->line->prev;
        --b->currentLine;
    }
    for (int i = 0; i < g_termRows - 1; ++i) {
        move(y + i, x);

        /* DRAW LINE NUMBERS IF THEY ARE ENABLED */
        attron(COLOR_PAIR(1));
        if (g_cb->conf->lineNumbers) {
            printw("%4d ", i + g_cb->yScroll + 1);
        }
        attroff(COLOR_PAIR(1));

        int colOffSet = (g_cb->conf->lineNumbers) ? 5 : 0;

        gb_cursesDraw(b->line->content, g_cb->xScroll, g_termCols - x - colOffSet);
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
    int x = b_columnNumber(b) + xOff - g_cb->xScroll;
    if (g_cb->conf->lineNumbers)
        x += 5;
    move(b->currentLine + yOff - g_cb->yScroll, x);
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

char b_charUnderCursor(Buffer *b)
{
    return gb_nextChar(b->line->content);
}

void b_forwardUntil(Buffer *b, char c, unsigned char lines, unsigned char atLeastOne)
{
    if (atLeastOne)
        b_cursorRight(b);
    if (lines) {
        while (1) {
            if (b_charUnderCursor(b) == c || (b->currentLine + 1 == b->numLines && b->line->content->ssz == 0))
                return;
            if (b->line->content->ssz == 0) {
                b_cursorDown(b);
                b_goToSOL(b);
            } else {
                b_cursorRight(b);
            }
        }
    } else {
        char cur;
        while ((cur = b_charUnderCursor(b))) {
            if (cur == c)
                return;
            b_cursorRight(b);

        }
    }
}

void b_backwardUntil(Buffer *b, char c, unsigned char lines, unsigned char atLeastOne)
{
    if (atLeastOne)
        b_cursorLeft(b);
    if (lines) {
        char cur;
        while ((cur = b_charUnderCursor(b))) {
            if (cur == c)
                return;
            if (b_columnNumber(b) == 0) {
                b_cursorUp(b);
                b_goToEOL(b);
            }
            b_cursorLeft(b);
        }
    } else {
        char cur;
        while ((cur = b_charUnderCursor(b))) {
            if (cur == c)
                return;
            if (b_columnNumber(b) == 0)
                return;
            b_cursorLeft(b);

        }
    }
}

void b_goToColumn(Buffer *b, size_t n)
{
    gb_forcePosition(b->line->content, n);
}

void b_insertSpaces(Buffer *b, unsigned char n)
{
    for (unsigned char i = 0; i < n; ++i)
        b_insertChar(b, ' ');
}

void b_forwardWord(Buffer *b)
{
    if (b_isAtEOL(b)) {
        if (b_isOnLastLine(b)) {
            b_goToEOL(b);
            return;
        } else {
            b_cursorDown(b);
            b_goToSOL(b);
            return;
        }
    }
    char current = b_charUnderCursor(b);
    while (current != ' ') {
        if (b_isAtEOF(b))
            return;
        if (b_isAtEOL(b)) {
            b_cursorDown(b);
            b_goToSOL(b);
            if (b_isAtEOL(b))
                return;
        } else {
            b_cursorRight(b);
        }
        current = b_charUnderCursor(b);
    }
    while (current == ' ') {
        if (b_isAtEOF(b))
            return;
        if (b_isAtEOL(b)) {
            b_cursorDown(b);
            b_goToSOL(b);
        } else {
            b_cursorRight(b);
        }
        current = b_charUnderCursor(b);
    }
}

unsigned char b_isAtEOL(Buffer *b)
{
    /* relies on implementation details of GAPBUFFER */
    return b->line->content->ssz == 0;
}

unsigned char b_isAtEOF(Buffer *b)
{
    return b_isOnLastLine(b) && b_isAtEOL(b);
}

unsigned char b_isOnLastLine(Buffer *b)
{
    return b->numLines == b->currentLine + 1;
}

void b_deleteUntilEOL(Buffer *b)
{
    gb_deleteUntilEOL(b->line->content);
}

void b_goToLine(Buffer *b, size_t line)
{
    if (line > b->currentLine) {
        while (b->currentLine < line && b->currentLine < b->numLines) {
            b_cursorDown(b);
        }
    } else {
        while (b->currentLine > line && b->currentLine > 0) {
            b_cursorUp(b);
        }
    }
}

void b_moveUp(Buffer *b, size_t n)
{
    size_t i = 0;
    while (b->currentLine > 0 && i < n) {
        b_cursorUp(b);
        ++i;
    }
}

void b_moveDown(Buffer *b, size_t n)
{
    size_t i = 0;
    while (b->currentLine < b->numLines && i < n) {
        b_cursorDown(b);
        ++i;
    }
}

void b_goToFirstNonWhitespaceCharOnLine(Buffer *b)
{
    b_goToSOL(b);
    b_forwardUntilNoneOf(b, " ");
}

void b_forwardUntilNoneOf(Buffer *b, char *chs)
{
    char current;
    while ((current = b_charUnderCursor(b))) {
        if (contains(chs, current))
            b_cursorRight(b);
        else
            return;
    }
    
}

unsigned char b_getPrevLineIndent(Buffer *b)
{
    if (!(b->line->prev))
        return 0;
    return gb_leadingSpaces(b->line->prev->content);
}
