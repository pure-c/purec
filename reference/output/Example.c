#include "Example.h"

PURS_ANY_THUNK_DECL(foo, PURS_ANY_INT(100))

PURS_ANY_THUNK_DECL(bar,
	PURS_ANY_BLOCK(() {
		return NULL;
	})
)
