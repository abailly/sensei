// from https://eradman.com/posts/tdd-in-c.html
#include <stdio.h>
#include <stdlib.h>
#include "sensei.h"

int tests_run = 0;

#define FAIL(msg) printf("\nfailure in %s() line %d: %s\n", __func__, __LINE__, msg)
#define _assert(test) do { if (!(test)) { FAIL(); return 1; } } while(0)
#define _assert_eq(a,b) do { if ((a) != (b)) { FAIL("expected " #a " but found " #b); return 1; } } while(0)
#define _assert_neq(a,b) do { if ((a) == (b)) { FAIL("expected " #b " to be different from " #a); return 1; } } while(0)
#define _verify(test) do { int r=test(); tests_run++; if(r) return r; } while(0)


int append_crlf_to_string_without_realloc_given_it_has_enough_capacity() {
  char *str;
  char *old_str = NULL;
  size_t capacity = 16;
  size_t new_len = 0;

  // allocate new string
  str = calloc(capacity, sizeof(char));
  old_str = str;

  // complete with less than capacity
  new_len = complete_with_crlf(&str, &capacity, 15);

  _assert_eq(old_str, str);
  _assert_eq(16, new_len);

  return 0;
}

int append_crlf_to_string_with_realloc_given_it_has_not_capacity() {
  char *str = NULL;
  size_t capacity = 16;
  size_t new_len = 0;

  // allocate new string
  str = malloc(capacity);

  // complete with equal capacity
  new_len = complete_with_crlf(&str, &capacity, 16);

  _assert_eq(17, new_len);

  return 0;
}

int returns_error_given_len_is_greater_than_capacity() {
  char *str = NULL;
  size_t capacity = 16;
  int new_len = 0;

  // complete with equal capacity
  new_len = complete_with_crlf(&str, &capacity, 17);

  _assert_eq(-1, new_len);

  return 0;
}

int parse_options_returns_error_if_no_option_given () {
  char *argv[] = {"senseic"};
  int argc = 1;
  client_options opts;
  int ret = 0;

  ret = parse_options(&opts, argc, argv);

  _assert_eq(-1, ret);
  return 0;
}

static char* sname = "foo.bar.dev";
static char* cpath = "--certificate-path";
static char* cpatharg = "server.cert";

int parse_options_returns_ok_and_fill_server_name_given_one_argument () {
  char *argv[] = {"senseic", sname};
  int argc = 2;
  client_options opts;
  int ret = 0;

  ret = parse_options(&opts, argc, argv);

  _assert_eq(0, ret);
  _assert_eq(sname, opts.server_name);

  return 0;
}

int parse_options_returns_ok_and_fill_certificate_path_with_2_args () {
  char *argv[] = {"senseic", cpath, cpatharg, sname };
  int argc = 4;
  client_options opts;
  int ret = 0;

  ret = parse_options(&opts, argc, argv);

  _assert_eq(0, ret);
  _assert_eq(sname, opts.server_name);
  _assert_eq(cpatharg, opts.certificate_path);

  return 0;
}

int parse_options_returns_ok_and_fill_certificate_path_with_2_args_reversed () {
  char *argv[] = {"senseic", sname, cpath, cpatharg};
  int argc = 4;
  client_options opts;
  int ret = 0;

  ret = parse_options(&opts, argc, argv);

  _assert_eq(0, ret);
  _assert_eq(sname, opts.server_name);
  _assert_eq(cpatharg, opts.certificate_path);

  return 0;
}

int parse_options_returns_ok_and_fill_certificate_path_with_2_short_args () {
  char *argv[] = {"senseic", "-c", cpatharg, sname };
  int argc = 4;
  client_options opts;
  int ret = 0;

  ret = parse_options(&opts, argc, argv);

  _assert_eq(0, ret);
  _assert_eq(sname, opts.server_name);
  _assert_eq(cpatharg, opts.certificate_path);

  return 0;
}

int all_tests() {
    _verify(append_crlf_to_string_without_realloc_given_it_has_enough_capacity);
    _verify(append_crlf_to_string_with_realloc_given_it_has_not_capacity);
    _verify(returns_error_given_len_is_greater_than_capacity);
    _verify(parse_options_returns_error_if_no_option_given);
    _verify(parse_options_returns_ok_and_fill_server_name_given_one_argument);
    _verify(parse_options_returns_ok_and_fill_certificate_path_with_2_args);
    _verify(parse_options_returns_ok_and_fill_certificate_path_with_2_args_reversed);
    _verify(parse_options_returns_ok_and_fill_certificate_path_with_2_short_args);
    return 0;
}

int main(int argc, char **argv) {
    int result = all_tests();
    if (result == 0)
        printf("PASSED\n");
    printf("Tests run: %d\n", tests_run);

    return result != 0;
}
