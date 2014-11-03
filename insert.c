#include "insert.h"
#include "state.h"
#include "buffer.h"
#include "normal.h"

void insertHandler(int c)
{
    switch (c) {
    case 27:
        g_mode = NORMAL;
        b_cursorLeft(g_cb);
        g_handleInput = normalHandler;
        break;
    case 13:
        b_insertChar(g_cb, '\n');
        break;
    case 8:
        b_backspace(g_cb);
        break;
    default:
        b_insertChar(g_cb, c);
    }
}
