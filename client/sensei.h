#ifndef SENSEI_H
#define SENSEI_H

#include <sys/types.h>

const char* SENSEI_VERSION;

typedef struct client_options {
  // name of server to connect to
  char* server_name;
  // (optional) path to server's certificate or certificate authority
  // used mostly for testing with self-signed certificates
  char* certificate_path;
} client_options;

size_t complete_with_crlf(char **buf, size_t *capacity, size_t len);

/* Parse command-line `client_options` and fill corresponding structure.
   Assumes argc == len(argv)
 */
int parse_options(client_options *opts, int argc, char **argv);

/* Display options from the command line */
void print_options(client_options *opts);

#endif // SENSEI_H
