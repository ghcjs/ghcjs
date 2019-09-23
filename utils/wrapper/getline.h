#if !HAVE_GETLINE

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

ssize_t
getdelim(char **buf, size_t *bufsiz, int delimiter, FILE *fp);

ssize_t
getline(char **buf, size_t *bufsiz, FILE *fp);

#endif
