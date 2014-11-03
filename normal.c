#include "normal.h"
#include "buffer.h"
#include "state.h"
#include "insert.h"
#include "quit.h"
#include "gapbuffer.h"

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
        gb_forcePosition(g_cb->b->line->content, g_cb->highCol);
        g_cb->visCol = gb_getPosition(g_cb->b->line->content);
        break;
    case KEY_MOVE_DOWN:
        b_cursorDown(g_cb->b);
        gb_forcePosition(g_cb->b->line->content, g_cb->highCol);
        g_cb->visCol = gb_getPosition(g_cb->b->line->content);
        break;
    case KEY_INSERT:
        g_mode = INSERT;
        g_handleInput = insertHandler;
        break;
    case KEY_APPEND:
        g_mode = INSERT;
        b_cursorRight(g_cb->b);
        g_handleInput = insertHandler;
        break;
    }
    /* If we are still in NORMAL mode, and the cursor is positioned after the last character on the
     * line, move the cursor to the left 1 character
     */
    if (g_mode == NORMAL) {
        if (gb_getPosition(g_cb->b->line->content) >= gb_length(g_cb->b->line->content)) {
            b_cursorLeft(g_cb->b);
            g_cb->highCol = MAX(gb_getPosition(g_cb->b->line->content), g_cb->highCol - 1);
        }
    }
}
