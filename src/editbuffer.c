#include "editbuffer.h"
#include "buffer.h"
#include "gapbuffer.h"
#include "config.h"
#include "normal.h"
#include "state.h"
#include "insert.h"
#include "visual.h"
#include "command.h"
#include <curses.h>

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

    /* set highCol to 0 so that the cursor stays in the first column if we scroll down */
    b->highCol = 0;

    /* set the edit buffer's handle based on the number of buffers already open */
    b->handle = g_numBuffers++;

    /* add the new EditBuffer to the global list of EditBuffers */
    g_bufList = realloc(g_bufList, sizeof (EditBuffer *) * g_numBuffers);
    g_bufList[g_numBuffers - 1] = b;

    b->currentIndent = 0;

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
    if (b->yScroll + g_termRows <= 2 + b->b->currentLine)
        b->yScroll = b->b->currentLine - g_termRows + 3; /* This 3 is to account for the command line and buffer list */

    /* adjust horizontal scroll if necessary */
    int numVisCols = g_termCols - (b->conf->lineNumbers ? 5 : 0);
    if (b->xScroll > gb_getPosition(b->b->line->content))
        b->xScroll = gb_getPosition(b->b->line->content);
    if (b->xScroll + numVisCols <= gb_getPosition(b->b->line->content))
        b->xScroll = gb_getPosition(b->b->line->content) - numVisCols + 1;
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
    if (b->conf->autoIndent)
        b_sameIndentAsAbove(b->b);

}

void buf_newLineAbove(EditBuffer *b)
{
    buf_goToSOL(b);
    b_insertChar(b->b, '\n');
    buf_prevLine(b, 1);
    if (b->conf->autoIndent)
        b_sameIndentAsBelow(b->b);
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

void buf_insertFNW(EditBuffer *b)
{
    b_goToFirstNonWhitespaceCharOnLine(b->b);
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

void buf_deleteCurrentLine(EditBuffer *b)
{
    b_deleteCurrentLine(b->b);
}

size_t buf_columnNumber(EditBuffer *b)
{
    return b_columnNumber(b->b);
}

int buf_goToColumn(EditBuffer *b, int n)
{
    gb_forcePosition(b->b->line->content, n);
    return gb_getPosition(b->b->line->content) == n;
}

void buf_commandMode(EditBuffer *b)
{
    b->mode = COMMAND;
    b->handleInput = commandHandler;
    buf_drawCommandLine(b);
    int c = getch();
    b->handleInput(c);
}

/* TODO make this actually print the command and not just a colon */
/* This is bad and needs to die */
void buf_drawCommandLine(EditBuffer *b)
{
    mvaddch(g_termRows - 2, 0, ':');
}

void buf_centerOnCurrentLine(EditBuffer *b)
{
    if (b->b->currentLine > g_termRows / 2)
        b->yScroll = b->b->currentLine - g_termRows / 2;
    else
        b->yScroll = 0;
}

char buf_charUnderCursor(EditBuffer *b)
{
    return b_charUnderCursor(b->b);
}

void buf_normalMode(EditBuffer *b)
{
    b->mode = NORMAL;
    b->handleInput = normalHandler;
    b_cursorLeft(b->b);
    b->highCol = gb_getPosition(b->b->line->content);
}

void buf_S(EditBuffer *b)
{
    unsigned char indent = b->conf->autoIndent ? b_getCurrentLineIndent(b->b) : 0;
    b_clearCurrentLine(b->b);
    b_insertSpaces(b->b, indent);
}

void buf_C(EditBuffer *b)
{
    b_deleteUntilEOL(b->b);
}

void buf_free(EditBuffer *b)
{
    free(b->fileName);
    free(b->conf);
    b_free(b->b);
    free(b);
}

/* if the current buffer is associated with a file, print the file's relative
 * name at the bottom of the screen, else print [No Name]
 */
void buf_drawFileName(EditBuffer *b)
{
    mvaddstr(g_termRows - 1, 0, b->fileName ? b->fileName : "[No Name]");
}

void buf_deleteCharUnderCursor(EditBuffer *b)
{
    if (b_currentLineIsEmpty(b->b))
        return;
    b_cursorRight(b->b);
    b_backspace(b->b);
}

/* put the EditBuffer into visual mode,
 * and reset the visual selection region
 * so that the current selection is just
 * the character under the cursor
 */
void buf_visualMode(EditBuffer *b)
{
    b->mode = VISUAL;
    b->handleInput = visualHandler;
    b->vs.startRow = b->b->currentLine;
    b->vs.startCol = b_columnNumber(b->b);
    b->vs.endRow = b->b->currentLine;
    b->vs.endCol = b_columnNumber(b->b);
}
