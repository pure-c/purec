#include "Example.h"

PURS_ANY_THUNK_DEF(foo, PURS_ANY_INT(100))

PURS_ANY_THUNK_DE(bar,
	PURS_ANY_BLOCK(() {

		return NULL;
	})
)
