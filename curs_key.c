#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <stdbool.h>

int main(int argc, char *argv[])
{
    initscr();
    raw();
    keypad(stdscr, true);
    int c;
    while (1) {
        c = getch();
        if (c == 'g')
            exit(0);
        mvprintw(0,0,"%d\n", c);
    }
    return 0;
}
