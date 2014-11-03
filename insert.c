#include "insert.h"
#include "state.h"
#include "buffer.h"
#include "normal.h"
#include <curses.h>

void insertHandler(int c)
{
    switch (c) {
    case 27:
        g_mode = NORMAL;
        b_cursorLeft(g_cb->b);
        g_handleInput = normalHandler;
        break;
    case 13:
        b_insertChar(g_cb->b, '\n');
        break;
    case KEY_BACKSPACE:
    case 127:
    case 8:
        b_backspace(g_cb->b);
        break;
    default:
        b_insertChar(g_cb->b, c);
    }
}
