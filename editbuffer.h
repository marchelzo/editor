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
    int visCol;
} EditBuffer;

EditBuffer *buf_new(void);

void buf_moveToLastCharOnCurrentLine(EditBuffer*);

int buf_isAtEOL(EditBuffer*);

EditorMode buf_mode(EditBuffer *b);

#endif
