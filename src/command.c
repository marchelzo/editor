#include <string.h>

#include "command.h"
#include "state.h"
#include "mode.h"
#include "quit.h"
#include "normal.h"
#include "bufwrite.h"
#include "commandcompletion.h"
#include "strdup.h"

static void appendToCommand(char c);
static void backspaceCommand(void);

void commandHandler(int c)
{
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
        case 9:
            break;
        case KEY_BACKSPACE:
        case 127:
        case 8:
            backspaceCommand();
            if (g_cb->mode == NORMAL)
                return;
            break;
        default:
            appendToCommand(c);
        }
        move(g_termRows - 1, 0);
        clrtoeol();
        printw(":%s", g_command);
        refresh();
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
    if (strcmp(com, "set autoindent") == 0) {
        g_cb->conf->autoIndent = 1;
        return;
    }
    if (strcmp(com, "set noautoindent") == 0) {
        g_cb->conf->autoIndent = 0;
        return;
    }
}

static void backspaceCommand(void)
{
    size_t len = strlen(g_command);
    if (len == 0) {
        g_cb->mode = NORMAL;
        g_cb->handleInput = normalHandler;
        return;
    }
    g_command = realloc(g_command, len);
    g_command[len - 1] = '\0';
}
