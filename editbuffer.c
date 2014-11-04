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
        if (fp) {
            b->b = b_fromFile(fp);
            fclose(fp);
            b->fileName = fn;
            return 0;
        } else {
            fp = fopen(fn, "w");
            if (fp) {
                b->fileName = fn;
                return 1;
            } else {
                return 2;
            }
        }
    } else {
        g_msg = "Unsaved text in buffer";
        return 3;
    }
}

void buf_goToLastCharOnCurrentLine(EditBuffer *b)
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
    buf_goToEOL(b);
    b_insertChar(b->b, '\n');

}

void buf_newLineAbove(EditBuffer *b)
{
    buf_goToSOL(b);
    b_insertChar(b->b, '\n');
    buf_prevLine(b, 1);
}

void buf_goToEOL(EditBuffer *b)
{
    b_goToEOL(b->b);
}

void buf_goToSOL(EditBuffer *b)
{
    b_goToSOL(b->b);
}

void buf_prevLine(EditBuffer *b, int n)
{
    int i = 0;
    while (i < n && b_prevLine(b->b) == 0)
        ++i;
}

void buf_appendEOL(EditBuffer *b)
{
    buf_goToEOL(b);
    buf_setMode(b, INSERT);
}

void buf_insertSOL(EditBuffer *b)
{
    buf_goToSOL(b);
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

char *buf_getCurrentLine(EditBuffer *b)
{
    return b_getCurrentLine(b->b);
}

size_t buf_currentLineNumber(EditBuffer *b)
{
    return b->b->currentLine;
}

size_t buf_numLines(EditBuffer *b)
{
    return b->b->numLines;
}

void buf_goToLine(EditBuffer *b, int n)
{
    if (n > b->b->currentLine) {
        buf_nextLine(b, n - b->b->currentLine);
    } else {
        buf_prevLine(b, b->b->currentLine - n);
    }
}

void buf_goToFirstLine(EditBuffer *b)
{
    buf_goToLine(b, 0);
}

void buf_goToLastLine(EditBuffer *b)
{
    buf_goToLine(b, b->b->numLines - 1);
}
