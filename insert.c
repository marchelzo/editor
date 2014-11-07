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
        halfdelay(2);
        c = getch();
        cbreak();
        if (c == 'k') {
            buf_normalMode(g_cb);
            break;
        }
        b_insertChar(g_cb->b, 'j');
        g_cb->handleInput(c);
        break;
    default:
        b_insertChar(g_cb->b, c);
    }
}
