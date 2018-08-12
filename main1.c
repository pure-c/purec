#include "Data.Maybe.h"
#include "Example.h"

#include <stdio.h> // Prelude FFI would include this (for sprintf)

// this would come from prelude and would be supplied by compiler
// XXX result could be 'static', thus saving us from reallocating block on every call
Data_Show_Show * Data_Maybe_showInt () {
	Data_Show_Show * value0 = GC_NEW(Data_Show_Show);
	value0->show =
		PURS_ANY_BLOCK((purs_any_t * i) {
			int * value0 = purs_any_get_int(i);
			if (value0 != NULL) {
				char * value1 = GC_MALLOC(100); // XXX real impl would live in Prelude FFI
				sprintf(value1, "%d", *value0);
				return purs_any_set_c_string(
					GC_NEW(purs_any_t),
					value1
				);
			}
			return (purs_any_t *) NULL; // XXX what now?
		});
	return value0;
}


int main () {
	purs_any_t * a = Data_Maybe_Just(foo());
	Data_Show_Show * x = Data_Maybe_showMaybe(Data_Maybe_showInt());
	purs_any_t * y = purs_any_app(x->show, a);
	printf("%s", y->value.c_string);
	return 0;
}
