#include <stdio.h>
#include <curses.h>

#include "buffer.h"
#include "state.h"
#include "mode.h"
#include "insert.h"
#include "normal.h"
#include "editbuffer.h"
#include "commandcompletion.h"

/* globals */
EditBuffer *g_cb;
char *g_msg;
char *g_command;
int g_termRows, g_termCols;
StringList *g_commandList;
HashMap *g_commandMap;

int main(int argc, char *argv[])
{
    g_cb = buf_new();
    int b = buf_loadFile(g_cb, argv[1]);

    compl_initializeGlobalCommandList();
    compl_addGlobalCommand("write");
    compl_addGlobalCommand("wait");
    compl_addGlobalCommand("windowNew");
    compl_addGlobalCommand("windowNewVeryLongCommandName");

    g_command = malloc(1);
    g_command[0] = '\0';

    initscr();
    clear();
    raw();
    cbreak();
    nonl();
    noecho();

    /* get the terminal dimensions */
    getmaxyx(stdscr, g_termRows, g_termCols);

    int c;
    b_cursesPrint(g_cb->b, 0, 0);
    b_cursesPositionCursor(g_cb->b, 0, 0);
    while (1) {
        c = getch();
        g_cb->handleInput(c);
        clear();
        buf_updateScrollPosition(g_cb);
        b_cursesPrint(g_cb->b, 0, 0);
        b_cursesPositionCursor(g_cb->b, 0, 0);
        refresh();
    }
    return 0;
}
