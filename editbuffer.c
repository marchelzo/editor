#include <stdbool.h>

#include "editbuffer.h"
#include "buffer.h"
#include "gapbuffer.h"
#include "config.h"
#include "normal.h"
#include "state.h"

EditBuffer *buf_new(void)
{
    EditBuffer *b = malloc(sizeof(EditBuffer));
    b->b = b_new();
    b->conf = malloc(sizeof(Config));
    b->mode = NORMAL;
    b->handleInput = normalHandler;
    b->fileName = NULL;
    return b;
}

int buf_loadFile(EditBuffer *b, char *fn)
{
    if (b_isEmpty(b->b)) {
        FILE *fp = fopen(fn, "r");
        if (!fp) {
            g_msg = "Failed to open file";
            return 1;
        }
        b->b = b_fromFile(fp);
        fclose(fp);
        b->fileName = fn;
    } else {
        g_msg = "Unsaved text in buffer";
        return 1;
    }
    return 0;
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
