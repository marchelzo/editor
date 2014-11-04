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
char *g_command;
int g_termRows, g_termCols;

int main(int argc, char *argv[])
{
    g_cb = buf_new();
    int b = buf_loadFile(g_cb, argv[1]);

    g_command = malloc(1);
    g_command[0] = '\0';

    initscr();
    clear();
    raw();
    nonl();
    noecho();

    /* get the terminal dimensions */
    getmaxyx(stdscr, g_termRows, g_termCols);

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
