#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include "runtime/purescript.h"

#include ".purec-work/lib/Main.h"

static void test(void **state) {
	(void) state; /* unused */
	purs_any_app(Main_main_$, purs_any_null);
}

int main (void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test),
	};

	return cmocka_run_group_tests(tests, NULL, NULL);
}
