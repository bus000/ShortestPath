#ifndef PROCESS_STATUS
#define PROCESS_STATUS

#include <inttypes.h>
#include <unistd.h>
#include "file.h"

#define MEMSTAT_EFORMAT (1)

/* A process memory usage information can be found in the file
 * /proc/{pid}/statm. The format of the file is,
 *
 * total program size |
 * resident set size |
 * shared pages |
 * text (code) |
 * data/stack |
 * library |
 * dirty pages |
 *
 * All the numbers are in number of pages used, a typical content of the file
 * is,
 *
 * 216304 13453 8145 23085 0 38685 0 */
typedef struct memory_usage_s {
    file_t memstat_file;

    uint32_t program_size; /* Number of pages allocated to program. */
    uint32_t resident_set_size;
    uint32_t shared_pages_size;
    uint32_t code_size; /* Number of pages used to store code. */
    uint32_t stack_size; /* Number of pages allocated to stack. */
    uint32_t library_size;
    uint32_t dirty_pages_size;
} memory_usage_t;

/* Initialize a new memory status monitor for the process with the pid given. */
/* Initialize a new memory status monitor for the process with the pid given.
 *
 * return 0 - Success.
 * return MEMSTAT_EFORMAT - Could not parse data from file. */
int memstat_init(memory_usage_t *memstat, pid_t pid);

/* Update the information of the memory usage of the process. */
int memstat_update(memory_usage_t *memstat);

/* Free the resources used by the memory usage status. */
void memstat_free(memory_usage_t *memstat);

#endif
