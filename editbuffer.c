#include <stdbool.h>

#include "editbuffer.h"
#include "buffer.h"
#include "gapbuffer.h"
#include "config.h"
#include "normal.h"

EditBuffer *buf_new(void)
{
    EditBuffer *b = malloc(sizeof(EditBuffer));
    b->b = b_new();
    b->conf = malloc(sizeof(Config));
    b->mode = NORMAL;
    b->handleInput = normalHandler;
    return b;
}

void buf_moveToLastCharOnCurrentLine(EditBuffer *b)
{
    gb_goToEnd(b->b->line->content);
    gb_moveLeft(b->b->line->content, 1);
}

int buf_isAtEOL(EditBuffer *b)
{
    return gb_length(b->b->line->content) == gb_getPosition(b->b->line->content);
}
