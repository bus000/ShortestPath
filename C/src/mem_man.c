#include "mem_man.h"
#include "file.h"
#include "error.h"
#include "bin_tree.h"

/* Contains all pointers allocated and all sizes of the corresponding
 * allocations. */
/*static void **pointers = NULL;*/
/*static uint32_t *sizes = NULL;*/
/*static uint32_t allocations = 0;*/
/*static uint32_t next = 0;*/

/*void init_mem_record(void)*/
/*{*/
    /*pointers = malloc(sizeof(void *) * 128);*/
    /*sizes = malloc(sizeof(uint32_t) * 128);*/
    /*allocations = 128;*/
    /*next = 0;*/

    /*if (pointers == NULL || sizes == NULL)*/
        /*error_code(ERR_NO_MEM, "Not enough space for memory allocation");*/
/*}*/

static file_t memmanfile;
static int initialized;

/*static bin_tree_t pointers;*/

void init_mem_record(char const *outfile)
{
    int ret = file_init(&memmanfile, outfile);

    if (ret != 0)
        error_code(ERR_FILE_OPEN, "Could not open file %s\n", outfile);

    initialized = 1;
}

void record_malloc(void const *var, size_t size)
{
    char string[128];

    if (initialized == 0)
        return;

    sprintf(string, "malloc %zu to %p\n", size, var);

    file_write(&memmanfile, string);
}

void record_realloc(void const *oldvar, void const *newvar, size_t newsize)
{
    char string[128];

    if (initialized == 0)
        return;

    sprintf(string, "realloc %zu to %p from %p\n", newsize, newvar, oldvar);

    file_write(&memmanfile, string);
}

void record_calloc(void const *var, size_t nmemb, size_t size)
{
    char string[128];

    if (initialized == 0)
        return;

    sprintf(string, "calloc %zu to %p\n", nmemb * size, var);

    file_write(&memmanfile, string);
}

void record_free(void const *var)
{
    fprintf(stderr, "Unsupported operating exception\n");
}

void free_mem_record(void)
{
    initialized = 0;
    file_free(&memmanfile);
}
