#include "normal.h"
#include "buffer.h"
#include "state.h"
#include "insert.h"
#include "quit.h"
#include "gapbuffer.h"
#include "command.h"
#include "mode.h"

#define KEY_MOVE_LEFT  'h'
#define KEY_MOVE_RIGHT 'l'
#define KEY_MOVE_UP    'k'
#define KEY_MOVE_DOWN  'j'

#define KEY_INSERT     'i'
#define KEY_APPEND     'a'

#define KEY_NL_BELOW   'o'
#define KEY_NL_ABOVE   'O'

#define KEY_APPEND_EOL 'A'
#define KEY_INSERT_SOL 'I'

#define KEY_COM_MODE   ';'

void normalHandler(int c)
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
    }
    if (g_cb->mode == NORMAL && buf_isAtEOL(g_cb))
        buf_goToLastCharOnCurrentLine(g_cb);
}

/* evaluates the given string by handling each character as if
 * it were pressed in normal mode.
 * Returns the global buffer to it's original mode after evaluting the normal
 * mode string
 */
void normalModeEval(const char *s)
{
    EditorMode m = g_cb->mode;

    g_cb->mode = NORMAL;
    g_cb->handleInput = normalHandler;

    while (*s) {
        g_cb->handleInput(*s);
        ++s;
    }

    buf_setMode(g_cb, m);
}
