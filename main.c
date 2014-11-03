#include <stdio.h>
#include <curses.h>

#include "buffer.h"
#include "state.h"
#include "mode.h"
#include "insert.h"
#include "normal.h"

/* globals */
EditorMode g_mode;
Buffer *g_cb;
void (*g_handleInput)(int);
char *g_msg;

int main(int argc, char *argv[])
{
    initscr();
    clear();
    raw();
    nonl();
    noecho();

    g_cb = b_new();
    g_mode = INSERT;
    g_handleInput = insertHandler;

    int c;
    while (1) {
        c = getch();
        g_handleInput(c);
        clear();
        b_cursesPrint(g_cb, 0, 0);

        mvprintw(20,20,"Current line: %d   pos: %d    num lines: %d", g_cb->currentLine, gb_getPosition(g_cb->line->content), g_cb->numLines);

        b_cursesPositionCursor(g_cb, 0, 0);

        refresh();
    }
    return 0;
}
