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

int all_tests() {
    _verify(append_crlf_to_string_without_realloc_given_it_has_enough_capacity);
    _verify(append_crlf_to_string_with_realloc_given_it_has_not_capacity);
    _verify(returns_error_given_len_is_greater_than_capacity);
    return 0;
}

int main(int argc, char **argv) {
    int result = all_tests();
    if (result == 0)
        printf("PASSED\n");
    printf("Tests run: %d\n", tests_run);

    return result != 0;
}
