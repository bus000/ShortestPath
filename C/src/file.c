#include "error.h"
#include "file.h"
#include "mem_man.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int file_init(file_t *file, char const *path)
{
    FILE *filepointer = fopen(path, "r");

    if (filepointer == NULL) {
        filepointer = fopen(path, "w+");
        if (filepointer == NULL)
            return -1;
    }

    file->file = filepointer;
    file->fileno = fileno(filepointer);
    file->content = NULL;
    file->path = strdup(path);

    return 0;
}

char * file_read(file_t *file)
{
    if (file->content == NULL)
        file_update(file);

    return strdup(file->content);
}

void file_update(file_t *file)
{
    int size = sizeof(char) * CHUNCK_SIZE, next = 0, cread;
    char *content;

    FREE(file->content);

    MALLOC(content, size);

    while (cread = read(file->fileno, &content[next], CHUNCK_SIZE)) {
        next += cread;

        if (cread == CHUNCK_SIZE) {
            size += CHUNCK_SIZE;
            REALLOC(content, size);
        }
    }

    file->content = content;

    /* Go back to beginning of file. */
    rewind(file->file);
}

int file_write(file_t *file, char const *content)
{
    fclose(file->file);
    file->file = fopen(file->path, "a");

    fputs(content, file->file);

    file->content = NULL;

    fclose(file->file);
    file->file = fopen(file->path, "r");

    return 0;
}

void file_free(file_t *file)
{
    if (file->content != NULL)
        FREE(file->content);

    fclose(file->file);
}
