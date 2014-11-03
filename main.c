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
void (*handleInput)(int);

int main(int argc, char *argv[])
{
    initscr();
    clear();
    raw();
    nonl();
    noecho();

    g_cb = b_new();
    g_mode = INSERT;
    handleInput = insertHandler;

    int c;
    while (1) {
        c = getch();
        handleInput(c);
        clear();
        b_cursesPrint(g_cb, 0, 0);
        if (g_mode == NORMAL)
            handleInput = normalHandler;
    }
    return 0;
}
