#ifndef __GAPBUFFER_H__
#define __GAPBUFFER_H__
    #include <stdlib.h>

    typedef struct {
        char *fst;
        char *snd;
        size_t fsz;
        size_t ssz;
    } GapBuffer;

    GapBuffer *gb_new(void);

    void gb_position(GapBuffer *gb, int p);

    int gb_getPosition(GapBuffer *gb);

    void gb_insertChar(GapBuffer *gb, char c);

    void gb_insertString(GapBuffer *gb, char *s);

    char *gb_substring(GapBuffer *gb, int begin, int end);

    size_t gb_length(GapBuffer *gb);

    char *gb_cString(GapBuffer *gb);

    void gb_delete(GapBuffer *gb, size_t);

    void gb_goToEnd(GapBuffer *gb);

    void gb_cursesPrint(GapBuffer *gb);

    void gb_free(GapBuffer *gb);

    void gb_moveLeft(int n);

    void gb_moveRight(int n);

#endif
