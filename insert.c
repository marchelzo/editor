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
        g_cb->mode = NORMAL;
        b_cursorLeft(g_cb->b);
        g_cb->handleInput = normalHandler;
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
        b_insertString(g_cb->b, "    ");
        break;
    default:
        b_insertChar(g_cb->b, c);
    }
}
