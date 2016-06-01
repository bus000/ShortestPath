#include "error.h"
#include "file.h"
#include "mem_man.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

void file_init(file_t *file, char const *path)
{
    FILE *filepointer = fopen(path, "r");

    if (filepointer == NULL) {
        filepointer = fopen(path, "w+");
        if (filepointer == NULL)
            error_code(ERR_FILE_OPEN, "Could not open file %s\n", path);
    }

    file->file = filepointer;
    file->fileno = fileno(filepointer);
    file->content = NULL;
    file->path = strdup(path);
}

char * file_read(file_t *file)
{
    if (file->content == NULL)
        file_update(file);

    if (file->content == NULL)
        return NULL;

    return strdup(file->content);
}

void file_update(file_t *file)
{
    struct stat statistics;

    if (file->content != NULL)
        FREE(file->content);

    if (fstat(file->fileno, &statistics) != 0)
        error_code(ERR_FILE_OPEN, "Could not get file status\n");

    MALLOC(file->content, statistics.st_size + 1); /* +1 for \0. */

    if (fread(file->content, sizeof(char), statistics.st_size, file->file) != statistics.st_size)
        error_code(ERR_FILE_OPEN, "Could not read file\n");

    /* Go back to beginning of file. */
    rewind(file->file);
}

int file_write(file_t *file, char const *content)
{
    fclose(file->file);
    file->file = fopen(file->path, "a");
    file->fileno = fileno(file->file);

    fputs(content, file->file);

    file->content = NULL;

    fclose(file->file);
    file->file = fopen(file->path, "r");
    file->fileno = fileno(file->file);

    return 0;
}

void file_free(file_t *file)
{
    if (file->content != NULL)
        FREE(file->content);

    fclose(file->file);
}
