#include "error.h"
#include "file.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

int file_init(file_t *file, char const *path)
{
    FILE *filepointer = fopen(path, "r");

    if (filepointer == NULL)
        return -1;

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

    free(file->content);

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

void file_free(file_t *file)
{
    if (file->content != NULL)
        free(file->content);

    fclose(file->file);
}
