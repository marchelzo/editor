#ifndef __BUFFER_H__
#define __BUFFER_H__

#include "gapbuffer.h"
#include <stdio.h>

typedef struct LN {
    struct LN *prev;
    struct LN *next;
    GapBuffer *content;
} LineNode;

typedef struct {
    LineNode *line;
    size_t currentLine;
    size_t numLines;
} Buffer;

Buffer *b_new(void);
Buffer *b_fromFile(FILE *f);

void b_insertLine(Buffer *b);
void b_insertChar(Buffer *b, char c);
void b_insertString(Buffer *b, const char *s);
void b_backspace(Buffer *b);
void b_joinLine(Buffer *b);
void b_deleteCurrentLine(Buffer *b);
void b_cursesPrint(Buffer *b, int xOff, int yOff);
void b_cursesPositionCursor(Buffer *b, int xOff, int yOff);
void b_cursorRight(Buffer *b);
void b_cursorLeft(Buffer *b);
void b_cursorUp(Buffer *b);
void b_cursorDown(Buffer *b);
void b_goToStart(Buffer *b);
int  b_nextLine(Buffer *b);
int  b_prevLine(Buffer *b);
void b_goToEOL(Buffer *b);
void b_goToSOL(Buffer *b);
char *b_getCurrentLine(Buffer *b);
size_t b_columnNumber(Buffer *b);

int b_isEmpty(Buffer *b);
#endif
