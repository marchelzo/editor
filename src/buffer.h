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
void b_insertSpaces(Buffer *b, unsigned char n);
void b_backspace(Buffer *b);
void b_joinLine(Buffer *b);
void b_deleteCurrentLine(Buffer *b);
void b_cursesDraw(Buffer *b, int xOff, int yOff);
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
char b_charUnderCursor(Buffer *b);
void b_forwardUntil(Buffer *b, char c, unsigned char lines, unsigned char atLeastOne);
void b_backwardUntil(Buffer *b, char c, unsigned char lines, unsigned char atLeastOne);
int b_isEmpty(Buffer *b);
void b_goToColumn(Buffer *b, size_t col);
void b_forwardWord(Buffer *b);
void b_deleteUntilEOL(Buffer *b);
unsigned char b_isAtEOL(Buffer *b);
unsigned char b_isAtEOF(Buffer *b);
unsigned char b_isOnLastLine(Buffer *b);
void b_goToLine(Buffer *b, size_t line);
void b_moveDown(Buffer *b, size_t n);
void b_moveUp(Buffer *b, size_t n);
void b_goToFirstNonWhitespaceCharOnLine(Buffer *b);
void b_forwardUntilNoneOf(Buffer *b, char *chs);
unsigned char b_getPrevLineIndent(Buffer *b);
unsigned char b_getNextLineIndent(Buffer *b);
unsigned char b_getCurrentLineIndent(Buffer *b);
void b_sameIndentAsAbove(Buffer *b);
void b_sameIndentAsBelow(Buffer *b);
void b_clearCurrentLine(Buffer *b);
void b_free(Buffer *b);
unsigned char b_currentLineIsEmpty(Buffer *b);
char *b_cString(Buffer *b);


#endif
