#ifndef DATA_MAYBE_H
#define DATA_MAYBE_H

#include <gc.h>
#include "purescript_runtime.h"

#define Data_Maybe_Nothing__Tag 1
#define Data_Maybe_Just__Tag 2

purs_any_t Data_Maybe_Nothing =
	{
		.tag = CONS,
		.value = {
			.cons = {
				.tag = Data_Maybe_Nothing__Tag
			}
		}
	};

purs_any_t * Data_Maybe_Just (purs_any_t * value0) {
	purs_any_t ** values = GC_MALLOC(sizeof (purs_any_t*[1]));
	values[0] = value0;
	return purs_any_set_cons(
		GC_NEW(purs_any_t),
		(purs_cons_t) {
			.tag = Data_Maybe_Just__Tag,
			.len = 1,
			.values = values
		}
	);
}

int main () {
	purs_any_t * x = Data_Maybe_Just(&Data_Maybe_Nothing);
	printf("%d\n", x->value.cons.values[0]->value.cons.tag);
	return 0;
}

#endif // DATA_MAYBE_H
