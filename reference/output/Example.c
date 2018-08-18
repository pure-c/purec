#include "Example.h"

// thunk top-level values
const purs_any_t * foo_fn (const purs_any_t * unused) {
	static const purs_any_t * v = NULL;
	if (v == NULL) {
		v = purs_any_set_int(
			GC_NEW(purs_any_t),
			100
		);
	}
	return v;
}

const purs_any_t foo_thunk = {
	.tag = THUNK,
	.value = {
		.fn = foo_fn
	}
};

const purs_any_t * foo = & foo_thunk;
