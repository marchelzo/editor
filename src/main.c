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
EditBuffer *g_cb = NULL;
EditBuffer **g_bufList = NULL;
int g_numBuffers;
char *g_msg = NULL;
char *g_command = NULL;
int g_termRows, g_termCols;
StringList *g_commandList = NULL;
HashMap *g_commandMap = NULL;

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

static void bufedit(int argc, char **argv)
{
    if (argc != 1)
        return;
    EditBuffer *b = buf_new();
    buf_loadFile(b, argv[0]);
    g_cb = b;
}

static void bufnext(int argc, char **argv)
{
    if (argc)
        return;
    if (g_cb->handle + 1 == g_numBuffers)
        return;
    g_cb = g_bufList[g_cb->handle + 1];
}

static void bufprev(int argc, char **argv)
{
    if (argc)
        return;
    if (g_cb->handle == 0)
        return;
    g_cb = g_bufList[g_cb->handle - 1];
}

int main(int argc, char *argv[])
{
    g_numBuffers = 0;
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
    hm_insert(g_commandMap, "e", bufedit);
    hm_insert(g_commandMap, "bnext", bufnext);
    hm_insert(g_commandMap, "bprev", bufprev);

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
