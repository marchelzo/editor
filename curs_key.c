#include <stdio.h>
#include <curses.h>

int main(int argc, char *argv[])
{
    initscr();
    raw();
    int c;
    while (1) {
        c = getch();
        mvprintw(0,0,"%d\n", c);
    }
    return 0;
}
