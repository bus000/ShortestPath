#include "mem_man.h"
#include "file.h"
#include "error.h"
#include "bin_tree.h"
#include <inttypes.h>
#include <time.h>
#include <pthread.h>

static file_t memmanfile;
static int initialized;

/* Record how much memory is allocated to each pointer in the program. */
static bin_tree_t pointers;

/* Contains the total amount of memory allocated to the program since the
 * recording was started. */
static uint64_t total_alloced_memory;

/* Locks. */
static pthread_t record_thread;
static pthread_mutex_t run_lock;
static int run;

static cmp_t cmp_pointers(void const *k1, void const *k2)
{
    return k1 < k2 ? LT : (k1 > k2 ? GT : EQ);
}

void * record_mem(void *outfile)
{
    time_t start = time(0), cur;
    char string[128];
    /* Sleep half a second. */
    struct timespec sleeptime = { .tv_sec = 0, .tv_nsec = 500000000 };

    pthread_mutex_lock(&run_lock);
    while (run) {
        pthread_mutex_unlock(&run_lock);

        cur = time(0);
        snprintf(string, 128, "%lld - %" PRIu64 "\n", (long long) cur - start,
                total_alloced_memory);
        file_write(&memmanfile, string);

        if (nanosleep(&sleeptime, NULL) != 0)
            error_report(ERR_SLEEP, "Could not sleep %lld\n nanoseconds",
                    sleeptime.tv_nsec);

        pthread_mutex_lock(&run_lock);
    }

    return NULL;
}

void init_mem_record(char const *outfile)
{
    int ret = file_init(&memmanfile, outfile);

    if (ret != 0)
        error_code(ERR_FILE_OPEN, "Could not open file %s\n", outfile);

    total_alloced_memory = 0;
    pointers = bin_tree_init(cmp_pointers);
    initialized = 1;

    if (pthread_mutex_init(&run_lock, NULL) != 0)
        error_code(ERR_CONCURRENT, "Could not init mutex %p\n", &run_lock);

    run = 1;

    pthread_create(&record_thread, NULL, record_mem, NULL);
}

void record_malloc(void const *var, size_t size)
{
    if (initialized == 0)
        return;

    printf("malloc %p\n", var);

    if (bin_tree_insert(&pointers, var, (int64_t) size) == BIN_TREE_CONFLICT)
        error_code(ERR_CONFLICT, "Two equal pointers %p returned by malloc.\n",
                var);

    total_alloced_memory += size;
}

void record_realloc(void const *oldvar, void const *newvar, size_t newsize)
{
    int64_t diff;
    int ret;
    int64_t oldval;

    if (initialized == 0)
        return;

    printf("realoc %p %p\n", oldvar, newvar);

    if (oldvar == newvar) {
        ret = bin_tree_update(&oldval, &pointers, oldvar, (int64_t) newsize);
        if (ret == BIN_TREE_NOT_FOUND)
            bin_tree_insert(&pointers, newvar, (int64_t) newsize);

    } else {
        ret = bin_tree_remove(&oldval, &pointers, oldvar);

        bin_tree_insert(&pointers, newvar, (int64_t) newsize);
    }

    if (ret == BIN_TREE_NOT_FOUND) {
        total_alloced_memory += (int64_t) newsize;
    } else {
        diff = oldval - ((int64_t) newsize);
        total_alloced_memory -= diff;
    }
}

void inline record_calloc(void const *var, size_t nmemb, size_t size)
{
    record_malloc(var, nmemb * size);
}

void record_free(void const *var)
{
    int64_t oldval;

    if (!initialized)
        return;

    printf("free %p\n", var);

    if (bin_tree_remove(&oldval, &pointers, var) != BIN_TREE_NOT_FOUND)
        total_alloced_memory -= oldval;
}

void free_mem_record(void)
{
    void *ret_val;

    initialized = 0;

    /* Stop thread recording memory usage. */
    pthread_mutex_lock(&run_lock);
    run = 0;
    pthread_mutex_unlock(&run_lock);

    pthread_join(record_thread, &ret_val);

    file_free(&memmanfile);
    bin_tree_free(&pointers);
}
