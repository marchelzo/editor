#include <string.h>

#include "command.h"
#include "state.h"
#include "mode.h"
#include "quit.h"
#include "normal.h"

static void appendToCommand(char c);

void commandHandler(int c)
{
    switch (c) {
    case 27:
        g_cb->mode = NORMAL;
        break;
    case 10:
    case 13:
        runCommand(g_command);
        free(g_command);
        g_command = malloc(1);
        g_command[0] = '\0';
        g_cb->mode = NORMAL;
        g_cb->handleInput = normalHandler;
        break;
    default:
        appendToCommand(c);
    }
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
    if (strcmp(com, "q") == 0) g_edit_quit();
}
