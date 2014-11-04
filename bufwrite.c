#include <stdio.h>
#include <stdlib.h>
#include "editbuffer.h"

int buf_write(EditBuffer *b)
{
    /* If the EditBuffer is not associated with a file on disk, return 1 indicating an error */
    if (b->fileName == NULL)
        return 1;
    
    /* save the current line so that we can return to it after writing the file */
    size_t currentLine = buf_currentLineNumber(b);

    size_t numLines = buf_numLines(b);

    /* move to the first line in the buffer */
    buf_goToFirstLine(b);

    /* move to the first character */
    buf_goToSOL(b);

    /* pointer that will point to each line in the buffer */
    char *line;

    /* open the file associated with the buffer for writing */
    FILE *fp = fopen(b->fileName, "w");

    /* return early if the file failed to open */
    if (fp == NULL)
        return 1;

    for (size_t i = 0; i < numLines - 1; ++i) {
        line = buf_getCurrentLine(b);
        fputs(line, fp);
        /* add a newline because the buffer representation ignores newlines */
        fputc('\n', fp);
        /* free the current line (a new C string is
         * allocated by buf_getCurrentLine each time it is called)
         */
        free(line);
        /* go to the next line */
        buf_nextLine(b, 1);
    }

    /* the last line in the buffer is a special case because no newline should be printed after it */
    line = buf_getCurrentLine(b);
    fputs(line, fp);
    free(line);

    /* write the final newline character */
    fputc('\n', fp);

    /* close the file */
    fclose(fp);

    /* go back to the line that we were on to begin with */
    buf_goToLine(b, currentLine);

    /* by some miracle, everything went according to plan and we can return 0 */
    return 0;
}
