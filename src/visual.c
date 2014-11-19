#include "visual.h"
#include "state.h"
#include "mode.h"
#include "gapbuffer.h"
#include "quit.h"
#include "keyaliases.h"
#include "insert.h"

void visualHandler(int c)
{
    switch (c) {
    case 27:
        g_edit_quit();
    case KEY_MOVE_RIGHT:
        b_cursorRight(g_cb->b);
        g_cb->highCol = gb_getPosition(g_cb->b->line->content);
        break;
    case KEY_MOVE_LEFT:
        b_cursorLeft(g_cb->b);
        g_cb->highCol = gb_getPosition(g_cb->b->line->content);
        break;
    case KEY_MOVE_UP:
        b_cursorUp(g_cb->b);
        if (g_cb->highCol >= gb_length(g_cb->b->line->content))
            buf_goToLastCharOnCurrentLine(g_cb);
        else
            gb_position(g_cb->b->line->content, g_cb->highCol);
        break;
    case KEY_MOVE_DOWN:
        b_cursorDown(g_cb->b);
        if (g_cb->highCol >= gb_length(g_cb->b->line->content))
            buf_goToLastCharOnCurrentLine(g_cb);
        else
            gb_position(g_cb->b->line->content, g_cb->highCol);
        break;
    case KEY_INSERT:
        g_cb->mode = INSERT;
        g_cb->handleInput = insertHandler;
        break;
    case KEY_APPEND:
        g_cb->mode = INSERT;
        b_cursorRight(g_cb->b);
        g_cb->handleInput = insertHandler;
        break;
    case KEY_NL_BELOW:
        buf_newLineBelow(g_cb);
        g_cb->mode = INSERT;
        g_cb->handleInput = insertHandler;
        break;
    case KEY_NL_ABOVE:
        buf_newLineAbove(g_cb);
        g_cb->mode = INSERT;
        g_cb->handleInput = insertHandler;
        break;
    case KEY_COM_MODE:
        buf_commandMode(g_cb);
        break;
    case KEY_APPEND_EOL:
        buf_appendEOL(g_cb);
        break;
    case KEY_INSERT_SOL:
        buf_insertSOL(g_cb);
        break;
    case 'g':
        c = getch();
        if (c == 'g') {
            buf_goToFirstLine(g_cb);
            buf_goToSOL(g_cb);
        }
        break;
    case 'G':
        buf_goToLastLine(g_cb);
        buf_goToSOL(g_cb);
        break;
    case 'd':
        c = getch();
        if (c == 'd')
            buf_deleteCurrentLine(g_cb);
        break;
    case 'z':
        c = getch();
        if (c == 'z')
            buf_centerOnCurrentLine(g_cb);
        break;
    case 'f':
        {
        c = getch();
        size_t col = buf_columnNumber(g_cb);
        b_forwardUntil(g_cb->b, c, 0, 1);
        if (b_charUnderCursor(g_cb->b) != c)
            b_goToColumn(g_cb->b, col);
        g_cb->highCol = buf_columnNumber(g_cb);
        break;
        }
    case 'F':
        {
        c = getch();
        size_t col = buf_columnNumber(g_cb);
        b_backwardUntil(g_cb->b, c, 0, 1);
        if (b_charUnderCursor(g_cb->b) != c)
            b_goToColumn(g_cb->b, col);
        g_cb->highCol = buf_columnNumber(g_cb);
        break;
        }
    case 'W':
        b_forwardWord(g_cb->b);
        g_cb->highCol = buf_columnNumber(g_cb);
        break;
    case '0':
        b_goToSOL(g_cb->b);
        g_cb->highCol = buf_columnNumber(g_cb);
        break;
    case 'D':
        b_deleteUntilEOL(g_cb->b);
        b_cursorLeft(g_cb->b);
        g_cb->highCol = buf_columnNumber(g_cb);
        break;
    case 4:
        b_moveDown(g_cb->b, g_termRows / 2);
        b_goToFirstNonWhitespaceCharOnLine(g_cb->b);
        g_cb->highCol = buf_columnNumber(g_cb);
        break;
    case 21:
        b_moveUp(g_cb->b, g_termRows / 2);
        b_goToFirstNonWhitespaceCharOnLine(g_cb->b);
        g_cb->highCol = buf_columnNumber(g_cb);
        break;
    case '^':
        b_goToFirstNonWhitespaceCharOnLine(g_cb->b);
        g_cb->highCol = buf_columnNumber(g_cb);
        break;
    case 'S':
        buf_S(g_cb);
        g_cb->mode = INSERT;
        g_cb->handleInput = insertHandler;
        break;
    case 'C':
        buf_C(g_cb);
        g_cb->mode = INSERT;
        g_cb->handleInput = insertHandler;
        break;
    case 'x':
        buf_deleteCharUnderCursor(g_cb);
    case C_RIGHT:
    case 559:
        /* TODO this does not need to be a hashmap lookup at runtime. make the bufnext / bufprev functions explicitly callable from here */
        hm_lookup(g_commandMap, "bnext")(0,NULL);
        break;
    case C_LEFT:
    case 544:
        hm_lookup(g_commandMap, "bprev")(0,NULL);
        break;
    }
    if (g_cb->mode != INSERT  && buf_isAtEOL(g_cb))
        buf_goToLastCharOnCurrentLine(g_cb);
    g_cb->vs.endRow = g_cb->b->currentLine;
    g_cb->vs.endCol = b_columnNumber(g_cb->b);
}
