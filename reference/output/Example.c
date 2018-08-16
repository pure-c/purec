#include "Example.h"

// thunk top-level values
purs_any_t * foo () {
	static purs_any_t * v = NULL;
	if (v == NULL) {
		v = purs_any_set_int(
			GC_NEW(purs_any_t),
			100
		);
	}
	return v;
}
