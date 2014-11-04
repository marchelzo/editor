#include <stdio.h>
#include <curses.h>

#include "buffer.h"
#include "state.h"
#include "mode.h"
#include "insert.h"
#include "normal.h"
#include "editbuffer.h"

/* globals */
EditBuffer *g_cb;
char *g_msg;

int main(int argc, char *argv[])
{
    initscr();
    clear();
    raw();
    nonl();
    noecho();

    g_cb = buf_new();
    buf_loadFile(g_cb, argv[1]);

    int c;
    while (1) {
        c = getch();
        g_cb->handleInput(c);
        clear();
        b_cursesPrint(g_cb->b, 0, 0);
        b_cursesPositionCursor(g_cb->b, 0, 0);
        refresh();
    }
    return 0;
}
