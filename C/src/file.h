#ifndef FILE_H
#define FILE_H

#include <stdio.h>

#define CHUNCK_SIZE (1024)

typedef struct file_s {
    FILE *file;
    int fileno;

    /* When a file is read, its content are saved in this pointer. The file is
     * not read automatically when creating a new file. So this pointer is
     * probably null. */
    char *content;

    /* Path of file when created. */
    char const *path;
} file_t;

/* Initialize a new file_t. If the file cannot be found, -1 is returned, 0 on no
 * error. */
int file_init(file_t *file, char const *path);

/* Read the file and return a pointer to the character array containing the
 * content. The string returned by this function should be freed by the
 * caller. */
char * file_read(file_t *file);

/* Reread the file pointed to by file. */
void file_update(file_t *file);

/* Free the resources used by the file and close file handles. */
void file_free(file_t *file);

#endif
