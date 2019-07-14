#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include "runtime/purescript.h"

#include ".purec-work/lib/Main.h"

static void leak_test(void **state) {
	(void) state; /* unused */
	assert_int_equal(0,
			 purs_any_get_int(purs_any_app(Main_main_$,
						       purs_any_null)));
}

int main (void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(leak_test),
	};

	return cmocka_run_group_tests(tests, NULL, NULL);
}
