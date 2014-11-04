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

void buf_moveToLastCharOnCurrentLine(EditBuffer*);

int buf_isAtEOL(EditBuffer*);

void buf_updateScrollPosition(EditBuffer *b);

EditorMode buf_mode(EditBuffer *b);

#endif
