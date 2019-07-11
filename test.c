#include "runtime/purescript.h"

ANY f(ANY ctx) {
	return ctx;
}

ANY g(ANY * ctx, ANY _, va_list __) {
	return purs_any_thunk_new(ctx[1], f);
}

ANY k() {
	ANY * ctx;

	ctx = purs_malloc(sizeof (ANY) * 2);
	ctx[0] = purs_any_int_new(1);
	ctx[1] = purs_any_int_new(3);

	return purs_any_cont_new(ctx, 2, g);
}

int main () {
	ANY x = k();
	return purs_any_get_int(purs_any_unthunk(purs_any_app(x, purs_any_null)));
}
