#include <stdlib.h>

typedef struct node_t {
    char *key;
    struct node_t *next;
    void (*action)(void);
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

typedef void (*fp)(void);

HashMap *hm_new(size_t numBuckets);

fp hm_lookup(HashMap *m, const char *key);

void hm_insert(HashMap *m, const char *key, fp value);
