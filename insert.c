#include "insert.h"
#include "state.h"
#include "buffer.h"

void insertHandler(int c)
{
    switch (c) {
    case 27:
        g_mode = NORMAL;
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
