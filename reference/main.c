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
				return PURS_ANY_STRING(afmt("%i", *value0));
			}
			assert (0); // XXX what now?
		});
	return value0;
}

int main () {
	const purs_any_t * a = Data_Maybe_Just(foo);
	const Data_Show_Show * x = Data_Maybe_showMaybe(Data_Maybe_showInt());
	const purs_any_t * y = purs_any_app(x->show, a);

	printf("%s\n", (char *) purs_any_get_string(y)->data);

	const purs_vec_t * v = purs_vec_new((const purs_any_t*[1]) { y }, 1);
	const purs_any_t * i = v->data[0];

	printf("%s\n", (char *) purs_any_get_string(i)->data);

	const purs_vec_t * v2 = purs_vec_copy(v);
	const purs_any_t * i2 = v2->data[0];

	printf("%s\n", (char *) purs_any_get_string(i2)->data);

	return 0;
}
