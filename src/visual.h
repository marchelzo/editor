#ifndef __VISUAL_H__
#define __VISUAL_H__

typedef struct {
    unsigned int startRow;
    unsigned int startCol;
    unsigned int endRow;
    unsigned int endCol;
} VisualSelection;

void visualHandler(int);

#endif
