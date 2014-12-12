#ifndef __MAPPINGS_H__
#define __MAPPINGS_H__

void mappings_init(void);

void mappings_setNormal(void);
void mappings_setInsert(void);
void mappings_setVisual(void);
void mappings_setCommand(void);

void mappings_newNormal(const char *to, const char *from);
void mappings_newInsert(const char *to, const char *from);
void mappings_newVisual(const char *to, const char *from);
void mappings_newCommand(const char *to, const char *from);

const StringList *mappings_getPossibilities(void);
void mappings_filter(int c);

#endif
