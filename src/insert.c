#include "insert.h"
#include "state.h"
#include "buffer.h"
#include "normal.h"
#include <curses.h>

#define KEY_TAB 9

void insertHandler(int c)
{
    switch (c) {
    case 27:
        buf_normalMode(g_cb);
        break;
    case 13:
        b_insertChar(g_cb->b, '\n');
        /* enter was pressed, so we check to see if autoindent is enabled. if it is, we indent. */
        if (g_cb->conf->autoIndent) {
            b_sameIndentAsAbove(g_cb->b);
        }
        break;
    case KEY_BACKSPACE:
    case 127:
    case 8:
        b_backspace(g_cb->b);
        break;
    case KEY_TAB:
        b_insertSpaces(g_cb->b, g_cb->conf->sw);
        break;
    case 'j':
        halfdelay(10);
        c = getch();
        nocbreak(); /* apparently this is necessary when using pdcurses (windows) ? */
        cbreak();
        if (c == 'k') {
            buf_normalMode(g_cb);
            break;
        }
        b_insertChar(g_cb->b, 'j');
        if (c != ERR)
            g_cb->handleInput(c);
        break;
    default:
        b_insertChar(g_cb->b, c);
    }
}
