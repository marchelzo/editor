#include "normal.h"
#include "buffer.h"
#include "state.h"

void normalHandler(int c)
{
    if (c == 27)
        exit(0);
}
