#include <stdbool.h>

#include "editbuffer.h"
#include "buffer.h"
#include "gapbuffer.h"
#include "config.h"
#include "normal.h"
#include "state.h"
#include "insert.h"
#include "command.h"

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

void buf_updateScrollPosition(EditBuffer *b)
{
    /* adjust vertical scroll if necessary */
    if (b->yScroll > b->b->currentLine)
        b->yScroll = b->b->currentLine;
    if (b->yScroll + g_termRows <= b->b->currentLine)
        b->yScroll = b->b->currentLine - g_termRows + 1;

    /* adjust horizontal scroll if necessary */
    if (b->xScroll > gb_getPosition(b->b->line->content))
        b->xScroll = gb_getPosition(b->b->line->content);
    if (b->xScroll + g_termCols <= gb_getPosition(b->b->line->content))
        b->xScroll = gb_getPosition(b->b->line->content) - g_termCols + 1;
}

void buf_nextLine(EditBuffer *b, int n)
{
    int i = 0;
    while (i < n && b_nextLine(b->b) == 0)
        ++i;
}

void buf_newLineBelow(EditBuffer *b)
{
    buf_moveToEOL(b);
    b_insertChar(b->b, '\n');

}

void buf_newLineAbove(EditBuffer *b)
{
    buf_moveToSOL(b);
    b_insertChar(b->b, '\n');
    buf_prevLine(b, 1);
}

void buf_moveToEOL(EditBuffer *b)
{
    b_moveToEOL(b->b);
}

void buf_moveToSOL(EditBuffer *b)
{
    b_moveToSOL(b->b);
}

void buf_prevLine(EditBuffer *b, int n)
{
    int i = 0;
    while (i < n && b_prevLine(b->b) == 0)
        ++i;
}

void buf_appendEOL(EditBuffer *b)
{
    buf_moveToEOL(b);
    buf_setMode(b, INSERT);
}

void buf_insertSOL(EditBuffer *b)
{
    buf_moveToSOL(b);
    buf_setMode(b, INSERT);
}

void buf_setMode(EditBuffer *b, EditorMode mode)
{
    switch (mode) {
    case INSERT:
        b->mode = INSERT;
        b->handleInput = insertHandler;
        break;
    case NORMAL:
        b->mode = NORMAL;
        b->handleInput = normalHandler;
        break;
    case COMMAND:
        b->mode = COMMAND;
        b->handleInput = commandHandler;
    }
}
