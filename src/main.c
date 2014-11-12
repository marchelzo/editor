#include <stdio.h>
#include <curses.h>

#include "buffer.h"
#include "state.h"
#include "mode.h"
#include "insert.h"
#include "normal.h"
#include "editbuffer.h"
#include "commandcompletion.h"
#include "quit.h"
#include "bufwrite.h"

/* globals */
EditBuffer *g_cb;
char *g_msg;
char *g_command;
int g_termRows, g_termCols;
StringList *g_commandList;
HashMap *g_commandMap;

static void quit(int argc, char **argv)
{
    g_edit_quit();
}

static void bufwrite(int argc, char **argv)
{
    buf_write(g_cb);
}

static void normalEvalHandler(int argc, char **argv)
{
    if (argc == 1) {
        normalModeEval(argv[0]);
    }
}

int main(int argc, char *argv[])
{
    g_cb = buf_new();
    g_cb->conf->lineNumbers = 1;
    g_cb->conf->sw = 4;
    g_cb->conf->autoIndent = 1;
    buf_loadFile(g_cb, argv[1]);

    /* compl_initializeGlobalCommandList(); */
    /* compl_addGlobalCommand("write"); */
    /* compl_addGlobalCommand("wait"); */
    /* compl_addGlobalCommand("windowNew"); */
    /* compl_addGlobalCommand("windowNewVeryLongCommandName"); */
    g_commandMap = hm_new(100);
    hm_insert(g_commandMap, "q", quit);
    hm_insert(g_commandMap, "w", bufwrite);
    hm_insert(g_commandMap, "normal", normalEvalHandler);

    g_command = malloc(1);
    g_command[0] = '\0';

    initscr();
    clear();
    raw();
    cbreak();
    nonl();
    noecho();
    start_color();
    use_default_colors();

    /* initialize some color pairs */
    init_pair(1, COLOR_YELLOW, -1); /*  on default bg */

    /* get the terminal dimensions */
    getmaxyx(stdscr, g_termRows, g_termCols);

    int c;
    b_cursesDraw(g_cb->b, 0, 0);
    b_cursesPositionCursor(g_cb->b, 0, 0);
    while (1) {
        c = getch();
        g_cb->handleInput(c);
        clear();
        buf_updateScrollPosition(g_cb);
        b_cursesDraw(g_cb->b, 0, 0);
        b_cursesPositionCursor(g_cb->b, 0, 0);
        refresh();
    }
    return 0;
}
