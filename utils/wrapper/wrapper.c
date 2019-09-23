/*
  Generic wrapper

  This program reads an options file (wrapper.exe.options in the same
  directory as wrapper.exe).

  The first line of the options files contains the wrapped executable
  to run. Each additional line contains an argument to prefix to the
  command line of the wrapped executable.

  Example (ghc.options):

  c:\Programs\ghc-8.6.5\lib\ghc.exe
  -Bc:\Programs\ghc-8.6.5\lib


  There are two special patterns:
 
  {{PUTENV}}

      If this pattern appears at the beginning of a line, the remainder
      of the line is passed to a putenv call.

      Example:
      
      {{PUTENV}}NODE_PATH=c:\node_modules

  {{EXEPATH}}

      The first occurrence of this pattern is substituted by the full path
      of the wrapper executable.
      
      Example:
 
      node
      {{EXEPATH}}\exename.jsexe\all.js

 */

#include "cwrapper.h"
#include "getLocation.h"
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#include "getline.h"

/*
  reads file_name line by line.
  updates numLines argument with number of lines
 */
char** readLines(const char *exe_path, const char *file_name, int *num_lines) {
    FILE *f;
    char *line_buf, *line_buf_tmp, *match;
    char **buf;
    int buf_size, lines_read, exe_path_len;
    ssize_t bytes_read;
    size_t line_buf_size;

    exe_path_len = strlen(exe_path);

    f = fopen(file_name, "r");
    if(!f) die("Could not read options file\n");
    buf_size = 10;
    lines_read = 0;
    buf = malloc(buf_size * sizeof(char*));
    if(buf == NULL) die("Malloc failed");
    while(1) {
        line_buf_size = 0;
        line_buf = NULL;
        bytes_read = getline(&line_buf, &line_buf_size, f);
        if(bytes_read == -1) {
            free(line_buf);
            break;
        }

        /* remove newline */
        if(bytes_read > 0 && line_buf[bytes_read-1] == '\n') {
            line_buf[bytes_read-1] = '\0';
        }

        /* expand {{EXEPATH}} (only once) */
        match = strstr(line_buf, "{{EXEPATH}}");
        if(match != NULL) {
            match[0] = '\0';
            line_buf_tmp = line_buf;
            line_buf = mkString("%s%s%s", line_buf_tmp, exe_path, match + 11);
            free(line_buf_tmp);
        }

        /* if line starts with {{PUTENV}}, interpret it */
        match = strstr(line_buf, "{{PUTENV}}");
        if(match == line_buf) {
            putenv(line_buf + 10);
            /* don't add the line to the arguments */
            free(line_buf);
            continue;
        }

        lines_read++;
        /* increase buffer size for number of lines if needed */
        if(lines_read >= buf_size) {
            buf_size *= 2;
            buf = realloc(buf, buf_size * sizeof(char*));
            if(buf == NULL) die("Realloc failed");
        }

        /* add line */
        buf[lines_read-1] = line_buf;
    }
    fclose(f);
    *num_lines = lines_read;
    return buf;
}

int main(int argc, char **argv) {
    char *exe_name     = getExecutable();
    char *exe_path     = getExecutablePath();
    char *options_name = mkString("%s.options", exe_name);
    int num_lines = 0;
    char **lines = readLines(exe_path, options_name, &num_lines);
    if(lines == NULL || num_lines == 0) die("Could not read options data");
    run(lines[0], num_lines - 1, lines + 1, argc - 1, argv + 1, NULL);
}
