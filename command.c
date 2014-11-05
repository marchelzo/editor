#include <string.h>

#include "command.h"
#include "state.h"
#include "mode.h"
#include "quit.h"
#include "normal.h"
#include "bufwrite.h"

static void appendToCommand(char c);

void commandHandler(int c)
{
    mvprintw(g_termRows - 1, 0, ":");
    refresh();
    do {
        switch (c) {
        case 27:
            g_cb->mode = NORMAL;
            g_cb->handleInput = normalHandler;
            free(g_command);
            return;
        case 10:
        case 13:
            runCommand(g_command);
            free(g_command);
            g_cb->mode = NORMAL;
            g_cb->handleInput = normalHandler;
            return;
        default:
            appendToCommand(c);
            mvprintw(g_termRows - 1,0,":%s", g_command);
        }
    } while ((c = getch()));
}

static void appendToCommand(char c)
{
    size_t len = strlen(g_command);
    g_command = realloc(g_command, len + 2);
    g_command[len] = c;
    g_command[len + 1] = '\0';
}

void runCommand(const char *com)
{
    if (strcmp(com, "q") == 0) {
        g_edit_quit();
        return;
    }
    if (strcmp(com, "w") == 0) {
        buf_write(g_cb);
        return;
    }
}
