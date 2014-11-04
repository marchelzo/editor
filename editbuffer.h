#ifndef __EDITBUFFER_H__
#define __EDITBUFFER_H__

#include "buffer.h"
#include "config.h"
#include "mode.h"

typedef struct {
    Buffer *b;
    void (*handleInput)(int);
    Config *conf;
    EditorMode mode;
    int highCol;
    int xScroll;
    int yScroll;
    char *fileName;
} EditBuffer;

EditBuffer *buf_new(void);

int buf_loadFile(EditBuffer *b, char *fn);

void buf_goToLastCharOnCurrentLine(EditBuffer*);

int buf_isAtEOL(EditBuffer*);

void buf_updateScrollPosition(EditBuffer *b);

EditorMode buf_mode(EditBuffer *b);

void buf_newLineAbove(EditBuffer *b);

void buf_newLineBelow(EditBuffer *b);

void buf_goToEOL(EditBuffer *b);

void buf_goToSOL(EditBuffer *b);

void buf_goToLine(EditBuffer *b, int n);

void buf_prevLine(EditBuffer *b, int n);

void buf_nextLine(EditBuffer *b, int n);

void buf_goToFirstLine(EditBuffer *b);

void buf_goToLastLine(EditBuffer *b);

void buf_setMode(EditBuffer *b, EditorMode mode);

void buf_appendEOL(EditBuffer *b);

void buf_insertSOL(EditBuffer *b);

char *buf_getCurrentLine(EditBuffer *b);

size_t buf_currentLineNumber(EditBuffer *b);

size_t buf_numLines(EditBuffer *b);

#endif
