#include "runtime/purescript.h"
#include "Data.Maybe.h"
#include "Example.h"

// this would come from prelude and would be supplied by compiler
// XXX result could be 'static', thus saving us from reallocating block on every call
const Data_Show_Show * Data_Maybe_showInt () {
	Data_Show_Show * value0 = GC_NEW(Data_Show_Show);
	value0->show =
		PURS_ANY_BLOCK((const purs_any_t * i) {
			const int * value0 = purs_any_get_int(i);
			if (value0 != NULL) {
				return purs_any_set_string(
					GC_NEW(purs_any_t),
					managed_utf8str_new(afmt("%i", *value0))
				);
			}
			assert (0); // XXX what now?
		});
	return value0;
}


int main () {
	const purs_any_t * a = Data_Maybe_Just(purs_any_unthunk(foo));
	const Data_Show_Show * x = Data_Maybe_showMaybe(Data_Maybe_showInt());
	const purs_any_t * y = purs_any_app(x->show, a);
	printf("%s\n", (char *) y->value.string->data);
	return 0;
}
