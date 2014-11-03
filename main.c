#define _GNU_SOURCE
#include <stdio.h>
#include <curses.h>

#include "buffer.h"
#include "state.h"
#include "mode.h"
#include "insert.h"
#include "normal.h"
#include "editbuffer.h"

/* globals */
EditorMode g_mode;
EditBuffer *g_cb;
void (*g_handleInput)(int);
char *g_msg;

int main(int argc, char *argv[])
{
    initscr();
    clear();
    raw();
    nonl();
    noecho();

    FILE *f = fopen(argv[1], "r");
    g_cb = malloc(sizeof(EditBuffer));
    g_cb->b = b_fromFile(f);
    g_mode = INSERT;
    g_handleInput = insertHandler;

    int c;
    while (1) {
        c = getch();
        g_handleInput(c);
        clear();
        b_cursesPrint(g_cb->b, 0, 0);
        b_cursesPositionCursor(g_cb->b, 0, 0);
        refresh();
    }
    return 0;
}
