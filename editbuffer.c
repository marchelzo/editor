#include "editbuffer.h"
#include "buffer.h"
#include "gapbuffer.h"
#include "config.h"

EditBuffer *buf_new(void)
{
    EditBuffer *b = malloc(sizeof(EditBuffer));
    b->b = b_new();
    b->conf = malloc(sizeof(Config));
    return b;
}


