#ifndef __EDITBUFFER_H__
#define __EDITBUFFER_H__

#include "buffer.h"
#include "config.h"

typedef struct {
    Buffer *b;
    Config *conf;
    int visCol;
    int highCol;
} EditBuffer;

#endif
