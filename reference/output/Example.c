#include "Example.h"

PURS_ANY_THUNK_DECL(foo,
	purs_any_set_int(
		GC_NEW(purs_any_t),
		100
	)
)

PURS_ANY_THUNK_DECL(bar,
	PURS_ANY_BLOCK(() {
		return NULL;
	})
)
