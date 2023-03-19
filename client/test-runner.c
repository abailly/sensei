// from https://eradman.com/posts/tdd-in-c.html
#include <stdio.h>
#include <math.h>

int tests_run = 0;

#define FAIL(msg) printf("\nfailure in %s() line %d: %s\n", __func__, __LINE__, msg)
#define _assert(test) do { if (!(test)) { FAIL(); return 1; } } while(0)
#define _assert_eq(a,b) do { if ((a) != (b)) { FAIL("expected " #a " but found " #b); return 1; } } while(0)
#define _verify(test) do { int r=test(); tests_run++; if(r) return r; } while(0)


int square_01() {
    int x=25;
    _assert_eq(sqrt(x), 6);
    return 0;
}

int all_tests() {
    _verify(square_01);
    return 0;
}

int main(int argc, char **argv) {
    int result = all_tests();
    if (result == 0)
        printf("PASSED\n");
    printf("Tests run: %d\n", tests_run);

    return result != 0;
}
