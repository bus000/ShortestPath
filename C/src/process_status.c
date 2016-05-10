#include "error.h"
#include "process_status.h"
#include "file.h"
#include <inttypes.h>
#include <unistd.h>

#define PATH_SIZE (128)

int memstat_init(memory_usage_t *memstat, pid_t pid)
{
    char status_path[PATH_SIZE];

    snprintf(status_path, PATH_SIZE, "/proc/%ld/statm", (long) pid);

    if (file_init(&memstat->memstat_file, status_path) != 0)
        error_code(ERR_FILE_OPEN, "Could not open the file %s\n", status_path);

    return memstat_update(memstat);
}

int memstat_update(memory_usage_t *memstat)
{
    int ret;

    file_update(&memstat->memstat_file);

    printf("content %s\n", memstat->memstat_file.content);

    ret = sscanf(memstat->memstat_file.content, "%" PRIu32 " %" PRIu32 " %"
            PRIu32 " %" PRIu32 " %" PRIu32 " %" PRIu32 " %" PRIu32 "\n",
            &memstat->program_size, &memstat->resident_set_size,
            &memstat->shared_pages_size, &memstat->code_size,
            &memstat->stack_size, &memstat->library_size,
            &memstat->dirty_pages_size);

    if (ret != 7) {
        error_report(ERR_FORMAT, "%s does not have correct format\n",
                memstat->memstat_file.path);

        return MEMSTAT_EFORMAT;
    }

    return 0;
}

void memstat_free(memory_usage_t *memstat)
{
    file_free(&memstat->memstat_file);
}
