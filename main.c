#include <purescript.h>

PURS_SCOPE_T(foo_scope_t, void, {
		int foo;
});

PURS_FFI_FUNC_2(foo, a, b, {
	foo_scope_t * scope = purs_new(foo_scope_t);
	scope->foo = 100;
	return purs_any_int_new(100);
});

int main () {
	const ANY * x = APP(foo, NULL);
	const ANY * y = APP(x, NULL);
	printf("hi: %i\n", y->value.i);
}
