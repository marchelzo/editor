#include <stdio.h>
#include <curses.h>
#include <string.h>
#include <HsFFI.h>

#include "gapbuffer.h"
#include "buffer.h"
#include "state.h"
#include "mode.h"
#include "insert.h"
#include "normal.h"
#include "editbuffer.h"
#include "commandcompletion.h"
#include "quit.h"
#include "bufwrite.h"
#include "strdup.h"
#include "mappings.h"
#include "lisp/EditorLisp_stub.h"
#include "istring.h"
#include "syntax_coloring.h"

/* globals */
EditBuffer *g_cb = NULL;
EditBuffer **g_bufList = NULL;
int g_numBuffers;
char *g_msg = NULL;
GapBuffer *g_command = NULL;
int g_termRows, g_termCols;
StringList *g_commandList = NULL;
HashMap *g_commandMap = NULL;
char *g_evalResult = NULL;

static void bufclose(int, char**);
static void handleInput(int c);
static void executeInput(const int *in);


static void quit(int argc, char **argv)
{
    /* if there is only one buffer open, just quit.
     * otherwise, close that buffer and continue running
     */
    if (g_numBuffers == 1)
        g_edit_quit();
    else
        bufclose(0, NULL);
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

static void bufclose(int argc, char **argv)
{
    /* TODO should this take an argument (buffer handle) instead of just closing the current buffer? who knows */
    if (argc)
        return;
    int handle = g_cb->handle;
    EditBuffer **newBufList = malloc(sizeof (EditBuffer *) * g_numBuffers - 1);
    for (int i = 0; i < handle; ++i) {
        newBufList[i] = g_bufList[i];
        newBufList[i]->handle = i;
    }
    for (int i = handle + 1; i < g_numBuffers; ++i) {
        newBufList[i - 1] = g_bufList[i];
        newBufList[i - 1]->handle = i - 1;
    }
    --g_numBuffers;
    free(g_bufList);
    g_bufList = newBufList;
    buf_free(g_cb);
    if (handle == 0) {
        /* we closed the first buffer, so we have to go to the next one */
        g_cb = g_bufList[0];
    } else {
        /* we did not close the first buffer, so it's safe to go to the previous one */
        g_cb = g_bufList[handle - 1];
    }
}

static void drawOpenBufferNames(void)
{
    int xOff = 0;
    move(g_termRows - 1, 0);
    for (int i = 0; i < g_numBuffers; ++i) {
        if (strlen(g_bufList[i]->fileName) + xOff > g_termCols) /* TODO this should account for the spaces between buffer names as well */
            break;
        if (i == g_cb->handle)
            attron(COLOR_PAIR(2));
        addstr(g_bufList[i]->fileName);
        addch(' ');
        xOff += strlen(g_bufList[i]->fileName) + 1;
        if (i == g_cb->handle)
            attroff(COLOR_PAIR(2));
    }
}

/* TODO this is nowhere near functional. fix it for corner cases */
static void colorSelection(void)
{
    unsigned char xOff = g_cb->conf->lineNumbers ? 5 : 0;
    LineNode *l = g_cb->b->line;
    int firstLineToColor = MAX(g_cb->yScroll, g_cb->vs.startRow);
    for (int i = g_cb->b->currentLine; i > firstLineToColor; --i)
        l = l->prev;
    for (unsigned int i = firstLineToColor; i <= g_cb->vs.endRow; ++i) {
            mvchgat(i - g_cb->yScroll, xOff, 1 + gb_length(l->content) - g_cb->xScroll, (COLOR_PAIR(3)), 3, NULL);
            l = l->next;
    }
}

void executeInput(const int *in)
{
    /* ungetch repeatedly on the string, backwards, so that when
     * we read it, it's in the proper order
     */
    size_t len = istrlen(in);
    for (int i = len - 1; i >= 0; --i)
        ungetch(in[i]);

    int ch;
    while (1) {
        nodelay(stdscr, TRUE);
        ch = getch();
        if (ch == ERR)
            break;
        else
            ungetch(ch);
        nodelay(stdscr, FALSE);
        g_cb->handleInput(getch());
    }
    nodelay(stdscr, FALSE);
}

void resetMappingMode(void)
{
    switch (g_cb->mode) {
    case NORMAL:
        mappings_setNormal();
        break;
    case INSERT:
        mappings_setInsert();
        break;
    case VISUAL:
        mappings_setVisual();
        break;
    case COMMAND:
        mappings_setCommand();
        break;
    }
}

/* wrapper for handling input that replaces keys with 
 * their re-mapped analogue if there exists one
 */
static void handleInput(int c)
{
    static int *buffer = NULL;
    static size_t len = 0;

    const int *mv; /* used for lookup results */

    if (!buffer) buffer = malloc(100 * sizeof(int));

    buffer[len++] = c;
    buffer[len]   = '\0';

    mappings_filter(c);

    const StringList *ps = mappings_getPossibilities();
    if (ps->length == 0) {
        executeInput(buffer);
        free(buffer);
        buffer = NULL;
        len = 0;
        resetMappingMode();
    } else if (ps->length == 1) {
        if (istrlen(ps->strings->string) == len) {
            executeInput(ps->strings->mapValue);
            free(buffer);
            buffer = NULL;
            len = 0;
            resetMappingMode();
        } else {
            halfdelay(10);
            int ch = getch();
            nocbreak();
            cbreak();
            if (ch == ERR) {
                executeInput(buffer);
                free(buffer);
                buffer = NULL;
                len = 0;
                resetMappingMode();
            } else {
                handleInput(ch);
            }
        }
    } else if (mv = sl_contains(ps, buffer)) {
        halfdelay(10);
        int ch = getch();
        nocbreak();
        cbreak();
        if (ch == ERR) {
            executeInput(mv);
            free(buffer);
            buffer = NULL;
            len = 0;
            resetMappingMode();
        } else {
            handleInput(ch);
        }
    }
}

int main(int argc, char *argv[])
{
    hs_init(&argc, &argv);
    g_numBuffers = 0;
    g_cb = buf_new();
    g_cb->conf->lineNumbers = 1;
    g_cb->conf->sw = 4;
    g_cb->conf->autoIndent = 1;
    char *arg = strdup(argv[1]);
    buf_loadFile(g_cb, arg);

    mappings_init();
    mappings_setNormal();

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
    g_command = gb_new();

    initscr();
    clear();
    raw();
    nonl();
    noecho();
    keypad(stdscr, true);
    start_color();
    use_default_colors();

    /* initialize some color pairs */
    init_pair(1, COLOR_YELLOW, -1); /* yellow on default bg */
    init_pair(2, COLOR_GREEN, -1); /* green on default bg */
    init_pair(3, COLOR_WHITE, COLOR_BLUE); /* white on blue (selected text) */

    /* red on solarized bg */
    init_color(12, 219 * 4, 51 * 4, 47 * 4);
    init_color(10, 0, 43 * 4, 54 * 4);
    init_pair(4, 12, 10);

    /* get the terminal dimensions */
    getmaxyx(stdscr, g_termRows, g_termCols);

    int c;
    b_cursesDraw(g_cb->b, 0, 0);
    b_cursesPositionCursor(g_cb->b, 0, 0);
    while (1) {

        c = getch();
        handleInput(c);

        /* clear the screen */
        erase();

        /* update the terminal dimensions in case they have changed */
        getmaxyx(stdscr, g_termRows, g_termCols);

        /* make sure we draw the right part of the buffer */
        buf_updateScrollPosition(g_cb);

        /* draw each line of the buffer that is inside of our scroll region */
        b_cursesDraw(g_cb->b, 0, 0);
        
        if (g_cb->mode != COMMAND)
            drawOpenBufferNames();

        if (g_cb->mode == VISUAL)
            colorSelection();

	if (g_evalResult)
	    mvaddstr(g_termRows - 2, 0, g_evalResult);


        /* put the cursor back where it should be (the drawing functions move the cursor) */
        b_cursesPositionCursor(g_cb->b, 0, 0);

        /* update the display so that it reflects the in-memory representation of the window */
        refresh();
    }
    return 0;
}
