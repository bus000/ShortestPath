#ifndef ERROR_H
#define ERROR_H

/* Print program usage information and exit. */
void usage(char const *program_name);

/* Print memory error and exit. */
void mem_err(void);

/* Print error from error code. */
void error_code(int error);

#endif
