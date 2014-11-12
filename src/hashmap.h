#include <stdlib.h>
#include "command.h"

typedef void (*CommandAction)(int, char**);

typedef struct node_t {
    char *key;
    struct node_t *next;
    CommandAction action;
} Node;

typedef struct {
    Node *head;
} Bucket;

typedef struct {
    Bucket *buckets;
    size_t numBuckets;
    size_t nonEmptyBuckets;
    size_t numItems;
} HashMap;


HashMap *hm_new(size_t numBuckets);

CommandAction hm_lookup(HashMap *m, const char *key);

void hm_insert(HashMap *m, const char *key, CommandAction value);
