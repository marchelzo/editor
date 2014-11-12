#ifndef __COMANND_H__
#define __COMANND_H__

typedef struct {
    int argc;
    char **argv;
    char *command;
} Command;

Command* parseCommand(char *str);
void commandHandler(int);
void runCommand(char*);

#endif
