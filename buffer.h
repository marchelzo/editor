#ifndef __BUFFER_H__
#define __BUFFER_H__
    #include "gapbuffer.h"

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

    void b_insertLine(Buffer *b);
    void b_insertChar(Buffer *b, char c);
    void b_backspace(Buffer *b);
    void b_joinLine(Buffer *b);
    void b_cursesPrint(Buffer *b, int x, int y);

#endif
