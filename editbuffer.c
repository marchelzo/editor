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

    /* start the new buffer in NORMAL mode */
    b->mode = NORMAL;
    b->handleInput = normalHandler;

    /* the new buffer is not yet associated with a file on disk */
    b->fileName = NULL;

    /* the cursor is at the first character in the buffer
     * so xScroll and yScroll are both set to 0
     */
    b->xScroll = 0;
    b->yScroll = 0;

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
