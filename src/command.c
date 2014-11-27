#include <string.h>

#include "command.h"
#include "state.h"
#include "mode.h"
#include "quit.h"
#include "normal.h"
#include "bufwrite.h"
#include "commandcompletion.h"
#include "strdup.h"
#include "lispbindings.h"

static void appendToCommand(char c);
static void backspaceCommand(void);

void commandHandler(int c)
{
    refresh(); /* make sure that the ':' is drawn at the bottom of the screen */
    do {
        switch (c) {
        case 27:
            g_cb->mode = NORMAL;
            g_cb->handleInput = normalHandler;
            free(g_command);
            return;
        case 10:
        case 13: {
            /* we save a pointer to the _current_ contents of g_cb, because running the command may modify g_cb (e.g. opening a new buffer) */
            EditBuffer *b = g_cb;
            runCommand(g_command);
            free(g_command);
            /* if the command left us in command mode, then go to normal mode */
            if (b && b->mode == COMMAND) {
                b->mode = NORMAL;
                b->handleInput = normalHandler;
            }
            return;
        }
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

void runCommand(char *com)
{
    /* if the command is lisp code, we run it through the interpreter */
    if (*com == '(') {
	evalLisp(com);
	return;
    }

    /* else we know that it is just a regular command, and so we parse it and execute it */
    Command *c = parseCommand(com);
    if (c == NULL)
        return; /* TODO INVALID INPUT -- COULD NOT PARSE */
    CommandAction a = hm_lookup(g_commandMap, c->command);
    if (a == NULL)
        return; /* TODO INVALID COMMAND */

    /* run the command with the arguments parsed */
    a(c->argc, c->argv);
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

Command* parseCommand(char *str)
{
    Command *c = malloc(sizeof(*c));
    if (c == NULL)
        return NULL;
    c->argc = 0;
    size_t argsAllocd = 0;
    size_t commandLen = 0;
    c->argv = NULL;
    c->command = NULL;
    while (*str != ' ') {
        if (str[0] == '\0')
            break;
        c->command = realloc(c->command, commandLen + 1);
        if (c->command == NULL)
            return NULL;
        c->command[commandLen] = str[0];
        ++commandLen;
        ++str;
    }
    c->command = realloc(c->command, commandLen + 1);
    c->command[commandLen] = '\0';
    ++str;
    if (str[0] == '\0')
        return c;
    while (*str != '\0') {
        size_t argLen = 0;
        c->argv = realloc(c->argv, (c->argc + 1) * sizeof(char*));
        c->argv[c->argc] = NULL;
        while (*str != ' ') {
            if (*str == '\0')
                break;
            c->argv[c->argc] = realloc(c->argv[c->argc], argLen + 1);
            c->argv[c->argc][argLen] = str[0];
            ++argLen;
            ++str;
        }
        c->argv[c->argc] = realloc(c->argv[c->argc], argLen + 1);
        c->argv[c->argc][argLen] = '\0';
        ++c->argc;
        if (*str == '\0')
            break;
        ++str;
    }
    return c;
}
