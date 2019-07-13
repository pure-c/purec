#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include "runtime/purescript.h"

static ANY go(const struct purs_scope * scope, ANY arg, va_list _) {
	const char * prefix = purs_any_get_string(scope->bindings[0])->data;
	const char * suffix = purs_any_get_string(arg)->data;
	return purs_any_string(purs_str_new("%s%s", prefix, suffix));
}

static ANY mk_prefix_cont (const char * prefix) {
	const purs_str_t * s = purs_str_new("%s", prefix);
	const struct purs_scope * scope = ({
		const purs_any_t x = purs_any_string(s);
		purs_scope_new(1, &x);
	});
	const purs_cont_t * cont = purs_cont_new(scope, go);
	PURS_RC_RELEASE(scope);
	PURS_RC_RELEASE(s);
	return purs_any_cont(cont);
}

static void leak_memory_test(void **state) {
	(void) state; /* unused */

	ANY cont = mk_prefix_cont("foo: ");
	const purs_str_t * s = purs_str_new("bar");

	ANY output = purs_any_app(cont, purs_any_string(s));
	PURS_ANY_RELEASE(&output);

	output = purs_any_app(cont, purs_any_string(s));
	PURS_ANY_RELEASE(&output);

	PURS_RC_RELEASE(s);
	PURS_ANY_RELEASE(&cont);
}

int main (void) {
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(leak_memory_test),
    };
    return cmocka_run_group_tests(tests, NULL, NULL);
}
