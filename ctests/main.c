#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include "runtime/purescript.h"

static void leak_memory_test(void **state) {
	(void) state;
	const purs_str_t * s = purs_str_new("foo: %s", "bar");
	const purs_any_t x = purs_any_string(s);
	PURS_ANY_RETAIN(&x);
	PURS_ANY_RELEASE(&x);
}

int main (void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(leak_memory_test),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);
}
