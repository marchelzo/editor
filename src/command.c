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

static void backspaceCommand(void);

void commandHandler(int c)
{
    refresh(); /* make sure that the ':' is drawn at the bottom of the screen */
    do {
        switch (c) {
        case 27:
            g_cb->mode = NORMAL;
            g_cb->handleInput = normalHandler;
            return;
        case 10:
        case 13: {
            /* we save a pointer to the _current_ contents of g_cb, because running the command may modify g_cb (e.g. opening a new buffer) */
            EditBuffer *b = g_cb;
            /* clear the old eval result / error message */
            free(g_evalResult);
            g_evalResult = NULL;
            /* make a temp C string to call runCommand */
            char *com = gb_cString(g_command);
            runCommand(com);
            gb_clear(g_command);
            free(com);
            /* if the command left us in command mode, then go to normal mode */
            if (b && b->mode == COMMAND) {
                b->mode = NORMAL;
                b->handleInput = normalHandler;
            }
            return;
        }
        case 258: /* down  */
            break;
        case 259: /* up    */
            break;
        case 260: /* left  */
            gb_moveLeft(g_command, 1);
            break;
        case 261: /* right */
            gb_moveRight(g_command, 1);
            break;
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
            gb_insertChar(g_command, c);
        }
        move(g_termRows - 2, 0); /* We print the command on the second last line */
        clrtoeol();
        char *commandString = gb_cString(g_command); /* make a temporary copy of g_command as a char* */
        unsigned char showColon = commandString[0] != '(';
        printw("%s%s", showColon ? ":" : "", commandString);
        move(g_termRows - 2, gb_getPosition(g_command) + showColon);
        free(commandString); /* free the temporary char* copy of g_command */
        refresh();
    } while ((c = getch()));
}

void runCommand(char *com)
{
    /* make a copy of the command as a C string */
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

    /* free the temporary C string */
    free(com);
}

static void backspaceCommand(void)
{
    size_t len = gb_length(g_command);
    if (len == 0) {
        g_cb->mode = NORMAL;
        g_cb->handleInput = normalHandler;
        return;
    }
    gb_delete(g_command, 1);
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
