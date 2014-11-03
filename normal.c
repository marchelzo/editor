#include "normal.h"
#include "buffer.h"
#include "state.h"
#include "insert.h"

#define KEY_MOVE_LEFT  'h'
#define KEY_MOVE_RIGHT 'l'
#define KEY_MOVE_UP    'k'
#define KEY_MOVE_DOWN  'j'

#define KEY_INSERT     'i'
#define KEY_APPEND     'a'

void normalHandler(int c)
{
    switch (c) {
    case 27:
        exit(0);
    case KEY_MOVE_RIGHT:
        b_cursorRight(g_cb);
        break;
    case KEY_MOVE_LEFT:
        b_cursorLeft(g_cb);
        break;
    case KEY_MOVE_UP:
        b_cursorUp(g_cb);
        break;
    case KEY_MOVE_DOWN:
        b_cursorDown(g_cb);
        break;
    case KEY_INSERT:
        g_mode = INSERT;
        g_handleInput = insertHandler;
        break;
    case KEY_APPEND:
        g_mode = INSERT;
        b_cursorRight(g_cb);
        g_handleInput = insertHandler;
        break;
    }
}
