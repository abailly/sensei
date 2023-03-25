#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#include "sensei.h"

size_t complete_with_crlf(char **buf, size_t *capacity, size_t len) {
  char *new_buffer = NULL;

  if(len > *capacity) return -1;

  /* resize buffer if we need to */
  if(len == *capacity) {
    new_buffer = realloc(*buf, *capacity + 1);
    if (!new_buffer) {
      fprintf(stderr, "failed to allocate buffer for sending %s\n", strerror(errno));
      exit(EXIT_FAILURE);
    }
    *buf = new_buffer;
  }

  /* assumes Unix EOL */
  (*buf)[len-1] = '\r';
  (*buf)[len] = '\n';

  return len + 1;
}

int parse_options (client_options *opts, int argc, char **argv) {
  int i = 0;

  if(argc <= 0) {
    return -1;
  }

  while(i < argc) {
    if (!strcmp(argv[i], "--certificate-path") || !strcmp(argv[i], "-c")) {
      opts->certificate_path = argv[++i];
    } else {
      opts->server_name = argv[i++];
    }
  }

  return 0;
}
