#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include "runtime/purescript.h"

#include ".purec-work/lib/Main.h"

static void test(void **state) {
	(void) state; /* unused */
	ANY tmp = purs_any_app(Main_main_$, purs_any_null);
	PURS_ANY_RELEASE(&tmp);
}

#define UNIT_TESTING
#ifdef UNIT_TESTING
int main (void) {
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test),
	};

	return cmocka_run_group_tests(tests, NULL, NULL);
}
#else
int main(void) {
	test(NULL);
}
#endif
