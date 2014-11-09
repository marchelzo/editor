#include "hashmap.h"
#include "strdup.h"

#include <stdlib.h>
#include <string.h>

static size_t hash(const unsigned char *str);

typedef void (*fp)(void);

HashMap *hm_new(size_t size)
{
    HashMap *m = malloc(sizeof(HashMap));
    m->buckets = malloc(sizeof(Bucket) * size);
    m->numBuckets = size;
    m->nonEmptyBuckets = 0;
    m->numItems = 0;
    for (size_t i = 0; i < size; ++i)
        m->buckets[i].head = NULL;
    return m;
}

void hm_insert(HashMap *m, const char* key, void (*value)(void))
{
    size_t bucket = hash(key) % m->numBuckets;
    if (m->buckets[bucket].head == NULL) {
        /* the bucket is empty */
        char *keyCopy = strdup(key);
        m->buckets[bucket].head = malloc(sizeof(Node));
        m->buckets[bucket].head->key = keyCopy;
        m->buckets[bucket].head->action = value;
        ++m->nonEmptyBuckets;
        ++m->numItems;
    } else {
        /* walk through the linked list to find the next available spots, checking for duplicate entries along the way */
        Node *n = m->buckets[bucket].head;
        /* hang on to the previous node as well, because if the key does not exist and we end at NULL, we still
         * need a way to reference the last allod'c node
         */
        Node *prev = NULL;
        while (n) {
            if (strcmp(n->key, key) == 0) {
                /* the key is already in the map, so we just update the value and return */
                n->action = value;
                return;
            } else {
                /* keys are different, so we just keep walking down the list */
                prev = n;
                n = n->next;
            }
        }
        /* the key was not in the map already, so we must add it */
        char *keyCopy = strdup(key);
        Node *new = malloc(sizeof(Node));
        new->next = NULL;
        new->key = keyCopy;
        new->action = value;

        /* insert the key-value pair into the linked list after the last node */
        prev->next = new;

        /* increment the item counter in the hashmap */
        ++m->numItems;
    }
}

fp hm_lookup(HashMap *m, const char *key)
{
    size_t bucket = hash(key) % m->numBuckets;
    Node *n = m->buckets[bucket].head;
    while (n) {
        if (strcmp(n->key, key) == 0)
            return n->action;
        else
            n = n->next;
    }
    return NULL;
}

unsigned char hm_contains(HashMap *m, const char *key)
{
    return hm_lookup(m, key) != NULL;
}

static size_t hash(const unsigned char *str)
{
    size_t hash = 5381;
    int c;

    while (c = *str++)
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}
